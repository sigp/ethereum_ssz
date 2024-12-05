use super::{read_offset, Decode, DecodeError, BYTES_PER_LENGTH_OFFSET};

pub struct ReverseSszDecoderBuilder<'a> {
    bytes: &'a [u8],
    is_ssz_fixed_len: bool,
    ssz_fixed_len: usize,
}

impl<'a> ReverseSszDecoderBuilder<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        Self {
            bytes,
            is_ssz_fixed_len: true,
            ssz_fixed_len: 0,
        }
    }

    pub fn register_type<T: Decode>(&mut self) -> Result<(), DecodeError> {
        self.register_type_parameterized(T::is_ssz_fixed_len(), T::ssz_fixed_len())
    }

    pub fn register_type_parameterized(
        &mut self,
        is_ssz_fixed_len: bool,
        ssz_fixed_len: usize,
    ) -> Result<(), DecodeError> {
        self.is_ssz_fixed_len &= is_ssz_fixed_len;
        self.ssz_fixed_len = self
            .ssz_fixed_len
            .checked_add(ssz_fixed_len)
            .ok_or(DecodeError::FixedBytesOverflow)?;
        Ok(())
    }

    pub fn build(self) -> Result<ReverseSszDecoder<'a>, DecodeError> {
        let (fixed, variable) = if self.is_ssz_fixed_len {
            if self.bytes.len() == self.ssz_fixed_len {
                (self.bytes, &[][..])
            } else {
                return Err(DecodeError::InvalidByteLength {
                    len: self.bytes.len(),
                    expected: self.ssz_fixed_len,
                });
            }
        } else {
            self.bytes.split_at_checked(self.ssz_fixed_len).ok_or(
                DecodeError::InvalidByteLength {
                    len: self.bytes.len(),
                    expected: self.ssz_fixed_len,
                },
            )?
        };

        Ok(ReverseSszDecoder {
            fixed,
            variable,
            variable_offset: fixed.len(),
        })
    }
}

pub struct ReverseSszDecoder<'a> {
    fixed: &'a [u8],
    variable: &'a [u8],
    variable_offset: usize,
}

impl<'a> ReverseSszDecoder<'a> {
    pub fn decode_last<I: Decode>(&mut self) -> Result<I, DecodeError> {
        self.decode_last_parameterized(I::is_ssz_fixed_len(), I::ssz_fixed_len(), I::from_ssz_bytes)
    }

    pub fn decode_last_parameterized<I, F: Fn(&[u8]) -> Result<I, DecodeError>>(
        &mut self,
        is_ssz_fixed_len: bool,
        ssz_fixed_len: usize,
        decoder: F,
    ) -> Result<I, DecodeError> {
        if is_ssz_fixed_len {
            let (remaining, item) = self.split_last_fixed(ssz_fixed_len)?;
            self.fixed = remaining;
            decoder(item)
        } else {
            let (remaining, offset) = self.split_last_fixed(BYTES_PER_LENGTH_OFFSET)?;
            self.fixed = remaining;
            let offset = read_offset(offset)?;
            let offset = offset
                .checked_sub(self.variable_offset)
                .ok_or(DecodeError::OffsetIntoFixedPortion(offset))?;
            let (remaining, value) = self.split_variable(offset)?;
            self.variable = remaining;
            decoder(value)
        }
    }

    fn split_last_fixed(&self, n: usize) -> Result<(&'a [u8], &'a [u8]), DecodeError> {
        let mid = self
            .fixed
            .len()
            .checked_sub(n)
            .ok_or(DecodeError::InvalidByteLength {
                len: self.fixed.len(),
                expected: n,
            })?;
        self.fixed
            .split_at_checked(mid)
            .ok_or(DecodeError::InvalidByteLength {
                len: self.fixed.len(),
                expected: mid,
            })
    }

    fn split_variable(&self, mid: usize) -> Result<(&'a [u8], &'a [u8]), DecodeError> {
        self.variable
            .split_at_checked(mid)
            .ok_or(DecodeError::OffsetOutOfBounds(mid))
    }

    pub fn finish(&self) -> Result<(), DecodeError> {
        if !self.fixed.is_empty() {
            Err(DecodeError::ExcessFixedBytes(self.fixed.len()))
        } else if !self.variable.is_empty() {
            Err(DecodeError::ExcessVariableBytes(self.variable.len()))
        } else {
            Ok(())
        }
    }
}

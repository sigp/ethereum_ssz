use ssz::{Decode, DecodeError, Encode};
use ssz_derive::{Decode, Encode};
use std::fmt::Debug;
use std::marker::PhantomData;

fn assert_encode<T: Encode>(item: &T, bytes: &[u8]) {
    assert_eq!(item.as_ssz_bytes(), bytes);
}

fn assert_encode_decode<T: Encode + Decode + PartialEq + Debug>(item: &T, bytes: &[u8]) {
    assert_encode(item, bytes);
    assert_eq!(T::from_ssz_bytes(bytes).unwrap(), *item);
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(enum_behaviour = "tag")]
enum TagEnum {
    A,
    B,
    C,
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(enum_behaviour = "union")]
enum TwoFixedUnion {
    U8(u8),
    U16(u16),
}

#[derive(PartialEq, Debug, Encode, Decode)]
struct TwoFixedUnionStruct {
    a: TwoFixedUnion,
}

#[test]
fn two_fixed_union() {
    let eight = TwoFixedUnion::U8(1);
    let sixteen = TwoFixedUnion::U16(1);

    assert_encode_decode(&eight, &[0, 1]);
    assert_encode_decode(&sixteen, &[1, 1, 0]);

    assert_encode_decode(&TwoFixedUnionStruct { a: eight }, &[4, 0, 0, 0, 0, 1]);
    assert_encode_decode(&TwoFixedUnionStruct { a: sixteen }, &[4, 0, 0, 0, 1, 1, 0]);
}

#[derive(PartialEq, Debug, Encode, Decode)]
struct VariableA {
    a: u8,
    b: Vec<u8>,
}

#[derive(PartialEq, Debug, Encode, Decode)]
struct VariableB {
    a: Vec<u8>,
    b: u8,
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(enum_behaviour = "transparent")]
enum TwoVariableTrans {
    A(VariableA),
    B(VariableB),
}

#[derive(PartialEq, Debug, Encode, Decode)]
struct TwoVariableTransStruct {
    a: TwoVariableTrans,
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(enum_behaviour = "union")]
enum TwoVariableUnion {
    A(VariableA),
    B(VariableB),
}

#[derive(PartialEq, Debug, Encode, Decode)]
struct TwoVariableUnionStruct {
    a: TwoVariableUnion,
}

#[test]
fn two_variable_trans() {
    let trans_a = TwoVariableTrans::A(VariableA {
        a: 1,
        b: vec![2, 3],
    });
    let trans_b = TwoVariableTrans::B(VariableB {
        a: vec![1, 2],
        b: 3,
    });

    assert_encode_decode(&trans_a, &[1, 5, 0, 0, 0, 2, 3]);
    assert_encode_decode(&trans_b, &[5, 0, 0, 0, 3, 1, 2]);

    assert_encode_decode(
        &TwoVariableTransStruct { a: trans_a },
        &[4, 0, 0, 0, 1, 5, 0, 0, 0, 2, 3],
    );
    assert_encode_decode(
        &TwoVariableTransStruct { a: trans_b },
        &[4, 0, 0, 0, 5, 0, 0, 0, 3, 1, 2],
    );
}

#[test]
fn trans_enum_error() {
    assert_eq!(
        TwoVariableTrans::from_ssz_bytes(&[1, 3, 0, 0, 0]).unwrap_err(),
        DecodeError::NoMatchingVariant,
    );
}

#[test]
fn two_variable_union() {
    let union_a = TwoVariableUnion::A(VariableA {
        a: 1,
        b: vec![2, 3],
    });
    let union_b = TwoVariableUnion::B(VariableB {
        a: vec![1, 2],
        b: 3,
    });

    assert_encode_decode(&union_a, &[0, 1, 5, 0, 0, 0, 2, 3]);
    assert_encode_decode(&union_b, &[1, 5, 0, 0, 0, 3, 1, 2]);

    assert_encode_decode(
        &TwoVariableUnionStruct { a: union_a },
        &[4, 0, 0, 0, 0, 1, 5, 0, 0, 0, 2, 3],
    );
    assert_encode_decode(
        &TwoVariableUnionStruct { a: union_b },
        &[4, 0, 0, 0, 1, 5, 0, 0, 0, 3, 1, 2],
    );
}

#[test]
fn tag_enum() {
    assert_encode_decode(&TagEnum::A, &[0]);
    assert_encode_decode(&TagEnum::B, &[1]);
    assert_encode_decode(&TagEnum::C, &[2]);
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(enum_behaviour = "union")]
enum TwoVecUnion {
    A(Vec<u8>),
    B(Vec<u8>),
}

#[test]
fn two_vec_union() {
    assert_encode_decode(&TwoVecUnion::A(vec![]), &[0]);
    assert_encode_decode(&TwoVecUnion::B(vec![]), &[1]);

    assert_encode_decode(&TwoVecUnion::A(vec![0]), &[0, 0]);
    assert_encode_decode(&TwoVecUnion::B(vec![0]), &[1, 0]);

    assert_encode_decode(&TwoVecUnion::A(vec![0, 1]), &[0, 0, 1]);
    assert_encode_decode(&TwoVecUnion::B(vec![0, 1]), &[1, 0, 1]);
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(struct_behaviour = "transparent")]
struct TransparentStruct {
    inner: Vec<u8>,
}

impl TransparentStruct {
    fn new(inner: u8) -> Self {
        Self { inner: vec![inner] }
    }
}

#[test]
fn transparent_struct() {
    assert_encode_decode(&TransparentStruct::new(42), &vec![42_u8].as_ssz_bytes());
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(struct_behaviour = "transparent")]
struct TransparentStructSkippedField {
    inner: Vec<u8>,
    #[ssz(skip_serializing, skip_deserializing)]
    skipped: PhantomData<u64>,
}

impl TransparentStructSkippedField {
    fn new(inner: u8) -> Self {
        Self {
            inner: vec![inner],
            skipped: PhantomData,
        }
    }
}

#[test]
fn transparent_struct_skipped_field() {
    assert_encode_decode(
        &TransparentStructSkippedField::new(42),
        &vec![42_u8].as_ssz_bytes(),
    );
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(struct_behaviour = "transparent")]
struct TransparentStructNewType(Vec<u8>);

#[test]
fn transparent_struct_newtype() {
    assert_encode_decode(
        &TransparentStructNewType(vec![42_u8]),
        &vec![42_u8].as_ssz_bytes(),
    );
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(struct_behaviour = "transparent")]
struct TransparentStructNewTypeSkippedField(
    Vec<u8>,
    #[ssz(skip_serializing, skip_deserializing)] PhantomData<u64>,
);

impl TransparentStructNewTypeSkippedField {
    fn new(inner: Vec<u8>) -> Self {
        Self(inner, PhantomData)
    }
}

#[test]
fn transparent_struct_newtype_skipped_field() {
    assert_encode_decode(
        &TransparentStructNewTypeSkippedField::new(vec![42_u8]),
        &vec![42_u8].as_ssz_bytes(),
    );
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(struct_behaviour = "transparent")]
struct TransparentStructNewTypeSkippedFieldReverse(
    #[ssz(skip_serializing, skip_deserializing)] PhantomData<u64>,
    Vec<u8>,
);

impl TransparentStructNewTypeSkippedFieldReverse {
    fn new(inner: Vec<u8>) -> Self {
        Self(PhantomData, inner)
    }
}

#[test]
fn transparent_struct_newtype_skipped_field_reverse() {
    assert_encode_decode(
        &TransparentStructNewTypeSkippedFieldReverse::new(vec![42_u8]),
        &vec![42_u8].as_ssz_bytes(),
    );
}

#[derive(PartialEq, Debug, Encode, Decode)]
struct StructWithMoreThanOneFieldAndFirstFieldAsBytes {
    bytes: Vec<u8>,
    length: u8,
}

#[test]
fn struct_with_more_than_one_field_and_first_field_as_bytes() {
    let test_struct = StructWithMoreThanOneFieldAndFirstFieldAsBytes {
        bytes: vec![42_u8],
        length: 7,
    };

    assert_encode_decode(&test_struct, &[5, 0, 0, 0, 7, 42]);
}

#[derive(PartialEq, Debug, Encode)]
#[ssz(struct_behaviour = "transparent")]
struct TransparentStructSkippedFieldEncodeOnly {
    inner: Vec<u8>,
    #[ssz(skip_serializing)]
    skipped: PhantomData<u64>,
}

// We implement Decode manually (without derive) so we can reuse `assert_encode_decode` in the test.
impl Decode for TransparentStructSkippedFieldEncodeOnly {
    fn is_ssz_fixed_len() -> bool {
        <TransparentStructSkippedField as Decode>::is_ssz_fixed_len()
    }

    fn ssz_fixed_len() -> usize {
        <TransparentStructSkippedField as Decode>::ssz_fixed_len()
    }

    fn from_ssz_bytes(bytes: &[u8]) -> Result<Self, ssz::DecodeError> {
        let value = TransparentStructSkippedField::from_ssz_bytes(bytes)?;
        Ok(Self {
            inner: value.inner,
            skipped: PhantomData,
        })
    }
}

// This is a regression test for deriving *just* Encode on a struct with a single field.
//
// Previously this was buggy because the derive macro expected the skipped field to be marked as
// `skip_deserializing` rather than `skip_serializing`.
#[test]
fn transparent_struct_newtype_skipped_encode_only() {
    assert_encode_decode(
        &TransparentStructSkippedFieldEncodeOnly {
            inner: vec![42_u8],
            skipped: PhantomData,
        },
        &vec![42_u8].as_ssz_bytes(),
    );
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(enum_behaviour = "compatible_union")]
enum TwoFixedCompatibleUnion {
    U8(u8),
    U16(u16),
}

#[derive(PartialEq, Debug, Encode, Decode)]
struct TwoFixedCompatibleUnionStruct {
    a: TwoFixedCompatibleUnion,
}

#[test]
fn two_fixed_compatible_union() {
    let eight = TwoFixedCompatibleUnion::U8(1);
    let sixteen = TwoFixedCompatibleUnion::U16(1);

    // Selectors should be 1 and 2
    assert_encode_decode(&eight, &[1, 1]);
    assert_encode_decode(&sixteen, &[2, 1, 0]);

    assert_encode_decode(
        &TwoFixedCompatibleUnionStruct { a: eight },
        &[4, 0, 0, 0, 1, 1],
    );
    assert_encode_decode(
        &TwoFixedCompatibleUnionStruct { a: sixteen },
        &[4, 0, 0, 0, 2, 1, 0],
    );
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(enum_behaviour = "compatible_union")]
enum TwoVariableCompatibleUnion {
    A(VariableA),
    B(VariableB),
}

#[derive(PartialEq, Debug, Encode, Decode)]
struct TwoVariableCompatibleUnionStruct {
    a: TwoVariableCompatibleUnion,
}

#[test]
fn two_variable_compatible_union() {
    let union_a = TwoVariableCompatibleUnion::A(VariableA {
        a: 1,
        b: vec![2, 3],
    });
    let union_b = TwoVariableCompatibleUnion::B(VariableB {
        a: vec![1, 2],
        b: 3,
    });

    // Selectors should be 1 and 2
    assert_encode_decode(&union_a, &[1, 1, 5, 0, 0, 0, 2, 3]);
    assert_encode_decode(&union_b, &[2, 5, 0, 0, 0, 3, 1, 2]);

    assert_encode_decode(
        &TwoVariableCompatibleUnionStruct { a: union_a },
        &[4, 0, 0, 0, 1, 1, 5, 0, 0, 0, 2, 3],
    );
    assert_encode_decode(
        &TwoVariableCompatibleUnionStruct { a: union_b },
        &[4, 0, 0, 0, 2, 5, 0, 0, 0, 3, 1, 2],
    );
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(enum_behaviour = "compatible_union")]
enum TwoVecCompatibleUnion {
    A(Vec<u8>),
    B(Vec<u8>),
}

#[test]
fn two_vec_compatible_union() {
    // Selectors should be 1 and 2
    assert_encode_decode(&TwoVecCompatibleUnion::A(vec![]), &[1]);
    assert_encode_decode(&TwoVecCompatibleUnion::B(vec![]), &[2]);

    assert_encode_decode(&TwoVecCompatibleUnion::A(vec![0]), &[1, 0]);
    assert_encode_decode(&TwoVecCompatibleUnion::B(vec![0]), &[2, 0]);

    assert_encode_decode(&TwoVecCompatibleUnion::A(vec![0, 1]), &[1, 0, 1]);
    assert_encode_decode(&TwoVecCompatibleUnion::B(vec![0, 1]), &[2, 0, 1]);
}

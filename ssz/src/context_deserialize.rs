use crate::{Bitfield, Fixed, Progressive, Variable};
use context_deserialize::ContextDeserialize;
use serde::{
    de::{Deserializer, Error},
    Deserialize,
};
use typenum::Unsigned;

impl<'de, C, N> ContextDeserialize<'de, C> for Bitfield<Variable<N>>
where
    N: Unsigned + Clone,
{
    fn context_deserialize<D>(deserializer: D, _context: C) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Bitfield::<Variable<N>>::deserialize(deserializer)
            .map_err(|e| D::Error::custom(format!("{:?}", e)))
    }
}

impl<'de, C, N> ContextDeserialize<'de, C> for Bitfield<Fixed<N>>
where
    N: Unsigned + Clone,
{
    fn context_deserialize<D>(deserializer: D, _context: C) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Bitfield::<Fixed<N>>::deserialize(deserializer)
            .map_err(|e| D::Error::custom(format!("{:?}", e)))
    }
}

impl<'de, C> ContextDeserialize<'de, C> for Bitfield<Progressive> {
    fn context_deserialize<D>(deserializer: D, _context: C) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Bitfield::<Progressive>::deserialize(deserializer)
            .map_err(|e| D::Error::custom(format!("{:?}", e)))
    }
}

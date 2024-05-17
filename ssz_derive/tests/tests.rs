use ssz::{Decode, DecodeError, Encode};
use ssz_derive::{Decode, Encode};
use ssz_types::{
    typenum::{self, Unsigned},
    BitVector,
};
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

// Shape tests from EIP.
#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(struct_behaviour = "stable_container")]
#[ssz(max_fields = "typenum::U8")]
struct Shape {
    side: Option<u16>,
    color: Option<u8>,
    radius: Option<u16>,
}

// Shape tests from EIP.
#[derive(PartialEq, Debug, Encode, Decode)]
//#[tree_hash(profile = "Shape")]
struct Square {
    side: u16,
    color: u8,
}

#[derive(PartialEq, Debug, Encode, Decode)]
#[ssz(struct_behaviour = "stable_container")]
#[ssz(max_fields = "typenum::U8")]
struct ShapeVec {
    side: Option<u16>,
    color: Option<u8>,
    radius: Option<ssz_types::VariableList<u8, typenum::U8>>,
}

#[test]
///Shape(side=None, color=1, radius=0x42)
/// 06014200
fn shape_1() {
    let shape = Shape {
        side: None,
        color: Some(1),
        radius: Some(42),
    };

    assert_encode_decode(&shape, &vec![6, 1, 42, 0]);
}

#[test]
/// Shape(side=0x42, color=1, radius=None)
/// 03420001
fn shape_2() {
    let shape = Shape {
        side: Some(42),
        color: Some(1),
        radius: None,
    };

    assert_encode_decode(&shape, &vec![3, 42, 0, 1]);
}

#[test]
/// Square(side=0x42, color=1)
/// 420001
fn square() {
    let square = Square { side: 42, color: 1 };

    assert_encode_decode(&square, &vec![42, 0, 1]);
}

#[test]
fn shape_3() {
    let shape = ShapeVec {
        side: None,
        color: Some(1),
        radius: Some(vec![1, 2, 3, 4].into()),
    };

    assert_encode_decode(&shape, &vec![6, 1, 5, 0, 0, 0, 1, 2, 3, 4]);
}

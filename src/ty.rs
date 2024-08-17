use std::str::FromStr;

use anyhow::anyhow;
use num_enum::{TryFromPrimitive, TryFromPrimitiveError};

/// Possible simple type classes
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum UnoSimpleTypeClass {
    Void = 0,
    Boolean = 2,
    Byte = 3,
    Short = 4,
    UnsignedShort = 5,
    Long = 6,
    UnsignedLong = 7,
    Hyper = 8,
    UnsignedHyper = 9,
    Float = 10,
    Double = 11,
    Char = 1,
    String = 12,
    Type = 13,
    Any = 14,
}

/// Possible complex type classes
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum UnoComplexTypeClass {
    Sequence = 20,
    Enum = 15,
    Struct = 17,
    Exception = 19,
    Interface = 22,
}

/// Type class of an UNO value
#[derive(Debug, PartialEq, Eq)]
pub enum UnoTypeClass {
    Simple(UnoSimpleTypeClass),
    Complex(UnoComplexTypeClass),
}

impl TryFromPrimitive for UnoTypeClass {
    type Primitive = u8;

    type Error = TryFromPrimitiveError<Self>;

    const NAME: &'static str = stringify!(UnoTypeClass);

    fn try_from_primitive(number: Self::Primitive) -> Result<Self, Self::Error> {
        if let Ok(value) = UnoSimpleTypeClass::try_from_primitive(number) {
            return Ok(Self::Simple(value));
        }

        if let Ok(value) = UnoComplexTypeClass::try_from_primitive(number) {
            return Ok(Self::Complex(value));
        }

        Err(TryFromPrimitiveError::new(number))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnoType {
    Simple(UnoSimpleTypeClass),
    Complex(UnoComplexType),
}

impl UnoType {
    pub fn from_str_simple(value: &str) -> Option<UnoSimpleTypeClass> {
        Some(match value {
            "type" => UnoSimpleTypeClass::Type,
            "void" => UnoSimpleTypeClass::Void,
            "boolean" => UnoSimpleTypeClass::Boolean,
            "char" => UnoSimpleTypeClass::Char,
            "byte" => UnoSimpleTypeClass::Byte,
            "string" => UnoSimpleTypeClass::String,
            "short" => UnoSimpleTypeClass::Short,
            "unsigned short" => UnoSimpleTypeClass::UnsignedShort,
            "long" => UnoSimpleTypeClass::Long,
            "unsigned long" => UnoSimpleTypeClass::UnsignedLong,
            "hyper" => UnoSimpleTypeClass::Hyper,
            "unsigned hyper" => UnoSimpleTypeClass::UnsignedHyper,
            "float" => UnoSimpleTypeClass::Float,
            "double" => UnoSimpleTypeClass::Double,
            "any" => UnoSimpleTypeClass::Any,
            _ => return None,
        })
    }

    pub fn from_str_complex(value: &str) -> anyhow::Result<UnoComplexType> {
        // Check for sequence type (i.e []string)
        if value.len() > 2 && value.starts_with('[') {
            let type_name = &value[2..];
            let sub_type = match Self::from_str(type_name) {
                Ok(value) => value,
                Err(err) => return Err(err),
            };

            if matches!(sub_type, UnoType::Simple(UnoSimpleTypeClass::Void)) {
                return Err(anyhow!("sequence type cannot contain void"));
            }

            return Ok(UnoComplexType::Sequence {
                value_ty: Box::new(sub_type),
            });
        }

        Err(anyhow!("unable to parse complex type"))
    }
}

impl FromStr for UnoType {
    type Err = anyhow::Error;

    /// https://www.openoffice.org/udk/common/man/typesystem.html
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        // Check fro a simple type
        if let Some(value) = Self::from_str_simple(value) {
            return Ok(UnoType::Simple(value));
        }

        // Check from a complex type
        Self::from_str_complex(value).map(UnoType::Complex)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnoComplexType {
    Sequence { value_ty: Box<UnoType> },
    Enum,
    Struct,
    Exception,
    Interface,
}

impl UnoComplexType {
    pub fn type_matches(&self, class_type: &UnoComplexTypeClass) -> bool {
        matches!(
            (self, class_type),
            (
                UnoComplexType::Sequence { .. },
                UnoComplexTypeClass::Sequence
            ) | (UnoComplexType::Enum, UnoComplexTypeClass::Enum)
                | (UnoComplexType::Struct, UnoComplexTypeClass::Struct)
                | (UnoComplexType::Exception, UnoComplexTypeClass::Exception)
                | (UnoComplexType::Interface, UnoComplexTypeClass::Interface)
        )
    }
}

#[cfg(test)]
mod test {
    use super::{UnoComplexType, UnoSimpleTypeClass, UnoType};
    use std::str::FromStr;

    #[test]
    fn test_boolean_sequence() {
        let value = "[]boolean";
        let ty = UnoType::from_str(value).unwrap();

        assert_eq!(
            ty,
            UnoType::Complex(UnoComplexType::Sequence {
                value_ty: Box::new(UnoType::Simple(UnoSimpleTypeClass::Boolean))
            })
        )
    }

    #[test]
    fn test_void_sequence() {
        let value = "[]void";
        let _err = UnoType::from_str(value).unwrap_err();
    }

    #[test]
    fn test_nested_sequence() {
        let value = "[][]boolean";
        let ty = UnoType::from_str(value).unwrap();
        assert_eq!(
            ty,
            UnoType::Complex(UnoComplexType::Sequence {
                value_ty: Box::new(UnoType::Complex(UnoComplexType::Sequence {
                    value_ty: Box::new(UnoType::Simple(UnoSimpleTypeClass::Boolean))
                }))
            })
        )
    }

    #[test]
    fn test_nested_unsigned_sequence() {
        let value = "[][]unsigned short";
        let ty = UnoType::from_str(value).unwrap();
        assert_eq!(
            ty,
            UnoType::Complex(UnoComplexType::Sequence {
                value_ty: Box::new(UnoType::Complex(UnoComplexType::Sequence {
                    value_ty: Box::new(UnoType::Simple(UnoSimpleTypeClass::UnsignedShort))
                }))
            })
        )
    }
}

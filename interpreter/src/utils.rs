use crate::types::NumericCelType;

pub fn usize_from_cel_number(cel_index: &NumericCelType) -> Result<usize, String> {
    match cel_index {
        NumericCelType::Int(i) if *i >= 0 => Ok(*i as usize),
        NumericCelType::UInt(i) => Ok(*i as usize),
        _ => {
            return Err(format!(
                "Index must be a positive integer, not {:?}",
                cel_index
            ))
        }
    }
}

use crate::types::NumericCelType;

pub fn str_index_from_cel_number(
    cel_index: &NumericCelType,
    strlen: usize,
) -> Result<usize, String> {
    let index = match cel_index {
        NumericCelType::Int(i) if *i >= 0 => *i as usize,
        NumericCelType::UInt(i) => *i as usize,
        _ => {
            return Err(format!(
                "Index must be a positive integer, not {:?}",
                cel_index
            ))
        }
    };

    if index > (strlen) {
        return Err(format!(
            "Index {} out of bounds for string of length {}",
            index, strlen
        ));
    }
    Ok(index)
}

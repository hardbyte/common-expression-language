use crate::types::NumericCelType;
use crate::utils::usize_from_cel_number;

pub fn str_index_from_cel_number(
    cel_index: &NumericCelType,
    strlen: usize,
) -> Result<usize, String> {
    let index = usize_from_cel_number(cel_index)?;

    if index > (strlen) {
        return Err(format!(
            "Index {} out of bounds. String length {}",
            index, strlen
        ));
    }
    Ok(index)
}

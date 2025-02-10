// This function returns how much icecream there is left in the fridge.
// If it's before 22:00 (24-hour system), then 5 scoops are left. At 22:00,
// someone eats it all, so no icecream is left (value 0). Return `None` if
// `hour_of_day` is higher than 23.
fn maybe_icecream(hour_of_day: u16) -> Option<u16> {
    // TODO: Complete the function body.
	match hour_of_day {
		0..=21 => Some(5), // ..= is used to specify a range of values; if the pattern matches any value within the range, this line's arm will execute
		22 => Some(0), // Meaning of Some(0): "I have a number, and it's value is 0"
		_ => None, // _ to match any unused value
	}
}

fn main() {
    // You can optionally experiment here.
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn raw_value() {
        // TODO: Fix this test. How do you get the value contained in the
        // Option?
        //let icecreams = maybe_icecream(12);
		if let Some(icecreams) = maybe_icecream(12) {
			assert_eq!(icecreams, 5); // Don't change this line.
		} else {
			println!("No ice cream available at this hour!");
		}
    }

    #[test]
    fn check_icecream() {
        assert_eq!(maybe_icecream(0), Some(5));
        assert_eq!(maybe_icecream(9), Some(5));
        assert_eq!(maybe_icecream(18), Some(5));
        assert_eq!(maybe_icecream(22), Some(0));
        assert_eq!(maybe_icecream(23), Some(0));
        assert_eq!(maybe_icecream(24), None);
        assert_eq!(maybe_icecream(25), None);
    }
}
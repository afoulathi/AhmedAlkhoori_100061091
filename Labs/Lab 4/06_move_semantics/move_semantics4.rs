fn main() {
    // You can optionally experiment here.
}

#[cfg(test)]
mod tests {
    // TODO: Fix the compiler errors only by reordering the lines in the test.
    // Don't add, change or remove any line.
    #[test]
    fn move_semantics4() {
        let mut x = Vec::new();
		// we cannot borrow x as mutable more than once within the same scope
        let y = &mut x;
        y.push(42);
		// y is no longer used so a new reference can be used
		let z = &mut x;
        z.push(13);
        assert_eq!(x, [42, 13]);
    }
}
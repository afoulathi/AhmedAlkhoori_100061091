// TODO: Fix the compiler error in this function.
fn fill_vec(vec: Vec<i32>) -> Vec<i32> {
    let mut vec = vec; // Changed to mutable

    vec.push(88); // vec must be mutable for this to work

    vec
}

fn main() {
    // You can optionally experiment here.
	let mut v = Vec::new(); // Create mutable empty vector by calling Vec::new() function
	v.push(3);
	println!("v after pushing: {:?}", v);
	println!("Length of v: {}", v.len());
	v.pop(); // Remove the last element from v
	println!("v after popping: {:?}", v);
	println!("Length of v: {}", v.len());
	let v = fill_vec(v); // After passing v into fill_vec, it no longer exists within main function
	println!("v after filling: {:?}", v);
	println!("Length of v: {}", v.len());
	// to run the test: cargo test move_semantics1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn move_semantics1() {
        let vec0 = vec![22, 44, 66];
        let vec1 = fill_vec(vec0);
        assert_eq!(vec1, vec![22, 44, 66, 88]);
    }
}
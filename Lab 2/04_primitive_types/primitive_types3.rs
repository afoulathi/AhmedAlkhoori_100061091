fn main() {
    // TODO: Create an array called `a` with at least 100 elements in it.
    let a: [i32; 100] = [1; 100]; // Declare an array that holds 100 elements, where each element is 32-bit signed number. `a` is initialized to 100 1's. 

    if a.len() >= 100 {
        println!("Wow, that's a big array!");
    } else {
        println!("Meh, I eat arrays like that for breakfast.");
        panic!("Array not big enough, more elements needed");
    }
}
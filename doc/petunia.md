# Petunia Documentation

Code execution starts at the function marked with the entry attribute.

Note that the entry function does not need to called 'main'. It could be declared as anything so long as the entry attribute is present.

```
@entry
pub fn main(): u8 // this function cannot be declared inline and must be public (pub)
    // ... 
end
```
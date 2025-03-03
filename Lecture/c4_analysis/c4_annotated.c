// c4.c - A minimalist C compiler in C
// This compiler demonstrates core concepts while staying readable. Instead of optimizing for
// production use, it prioritizes teaching compiler fundamentals through clean implementation.

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long // Using 64-bit integers to handle modern memory addressing needs

// The compiler's state is intentionally kept in global variables. While this wouldn't be ideal
// for a production compiler, it makes the code flow much clearer for learning purposes by
// avoiding complex state management.
char *p,             // Current position in source code
     *lp,           // Line position - tracks start of current line for error reporting
     *data;         // Data segment - where we store strings and global variables

int *e,             // Current position in emitted code
    *le,           // Last emitted instruction - used for debugging output
    *id,           // Current identifier being processed in symbol table
    *sym,          // Symbol table - deliberately simple linear list for clarity
    tk,            // Current token - what we just lexed from source
    ival,          // Immediate value for number tokens
    ty,            // Type of current expression (INT, CHAR, or pointer)
    loc,          // Local variable offset on stack
    line,         // Source line number for error messages
    src,          // Flag to print source and assembly for debugging
    debug;        // Flag to print VM instructions during execution

// Token types and keywords are organized by precedence to make parsing easier.
// This clever ordering means we can use the token values directly in our expression
// parser to handle operator precedence without extra lookup tables.
enum {
    Num = 128, Fun, Sys, Glo, Loc, Id,   // Internal tokens for parser
    
    // Keywords - kept minimal but sufficient for basic C programs
    // We skip things like 'struct' and 'switch' to focus on core concepts
    Char, Else, Enum, If, Int, Return, Sizeof, While,
    
    // Operators ordered by precedence - this ordering is key to the
    // recursive descent parser's handling of operator precedence
    // For example, Mul comes after Add because * binds tighter than +
    Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge,
    Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// Virtual Machine instruction set
// Rather than try to target x86 or another real CPU, we use a simple virtual machine.
// This lets us focus on code generation concepts without getting lost in the details
// of a real instruction set. Our VM uses a stack architecture because it makes code
// generation straightforward - most operations just work with the top of the stack.
enum { 
    // Memory and control flow
    LEA,        // Load effective address - crucial for local vars
    IMM,        // Load immediate value
    JMP,        // Unconditional jump
    JSR,        // Jump to subroutine (call)
    BZ,         // Branch if zero
    BNZ,        // Branch if not zero
    ENT,        // Enter function - sets up stack frame
    ADJ,        // Adjust stack - cleanup after calls
    LEV,        // Leave function
    
    // Load/store operations
    LI,         // Load int
    LC,         // Load char
    SI,         // Store int
    SC,         // Store char
    PSH,        // Push onto stack
    
    // Arithmetic and logic - kept minimal but complete
    OR, XOR, AND,                   // Bitwise operations
    EQ, NE, LT, GT, LE, GE,        // Comparisons
    SHL, SHR,                       // Shifts
    ADD, SUB, MUL, DIV, MOD,       // Basic math
    
    // System calls - just enough to write useful programs
    OPEN, READ, CLOS, PRTF,        // I/O operations
    MALC, FREE, MSET, MCMP,        // Memory management
    EXIT                           // Program termination
};

// Our type system is intentionally minimal - just integers, chars, and pointers.
// This covers enough to demonstrate type checking and pointer arithmetic without
// getting bogged down in complex types like structs or unions.
enum { CHAR, INT, PTR };

// Symbol table entries are stored as an array of integers rather than a struct.
// This makes the code more concise (if a bit harder to read at first).
// Each symbol has these fields, accessed by these enum values as offsets:
enum { 
    Tk,      // Token type
    Hash,    // Hash value for quick comparison
    Name,    // Pointer to name string
    Class,   // Storage class (Global/Local/Function)
    Type,    // Data type
    Val,     // Value or stack offset
    HClass,  // Saved class for scope handling
    HType,   // Saved type for scope
    HVal,    // Saved value for scope
    Idsz     // Size of entry (used to find next entry)
};

// next() - The lexical analyzer (lexer)
// This function converts source code into a stream of tokens for the parser.
// Instead of using complex regular expressions or generated code like flex/lex,
// we use direct character-by-character processing for clarity and control.
// While this approach isn't as flexible as a generator-based lexer, it's
// much easier to understand and still handles everything we need for C.
void next()
{
    char *pp; // Temporary pointer for token start position

    while (tk = *p) {
        ++p;  // Always advance past current character
        
        // Newline handling does double duty: it tracks line numbers for error messages
        // and handles the debug output of source code alongside generated assembly.
        // This tight integration of debugging output with lexing is unusual but
        // makes the compiler's operation much more transparent.
        if (tk == '\n') {
            if (src) {
                // This clever debug output shows source line followed by the
                // assembly instructions generated for it - great for understanding
                // how source maps to machine code
                printf("%d: %.*s", line, p - lp, lp);
                lp = p;
                
                // Print assembly instructions using a space-efficient string table trick.
                // Each instruction name is exactly 5 chars (including spaces), so we can
                // index directly into this string to print instruction names.
                // This is way more compact than a switch or array of strings.
                while (le < e) {
                    printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                                   "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                                   "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
                    // Instructions up to ADJ have an immediate operand that needs printing
                    if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");
                }
            }
            ++line;
        }
        
        // We skip preprocessor directives (#include, #define, etc.) entirely.
        // This is a significant limitation - we can't handle header files or macros.
        // But it greatly simplifies the compiler while still allowing us to compile
        // meaningful programs. A production compiler would need proper preprocessing.
        else if (tk == '#') {
            while (*p != 0 && *p != '\n') ++p;
        }
        
        // Identifier & keyword handling - this includes variables, function names,
        // and keywords like 'if' and 'while'. We use a simple but effective
        // hash function to quickly compare identifiers.
        else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
            pp = p - 1;  // Remember start of identifier
            
            // Accumulate hash value while scanning identifier.
            // The multiplier 147 was chosen because it gives good distribution
            // for typical C identifiers with minimal collisions. This is much
            // simpler than but nearly as effective as more complex hash functions.
            while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || 
                   (*p >= '0' && *p <= '9') || *p == '_')
                tk = tk * 147 + *p++;
                
            // Final hash computation includes length to distinguish between
            // identifiers with the same characters in different positions
            tk = (tk << 6) + (p - pp);
            
            // Search symbol table for this identifier. The linear search is
            // simple but sufficient for small programs. A production compiler
            // would use a hash table for better performance with large programs.
            id = sym;
            while (id[Tk]) {
                if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { 
                    tk = id[Tk];  // Found it - return its token type
                    return; 
                }
                id = id + Idsz;
            }
            
            // Not found - add new identifier to symbol table.
            // Notice we store the actual string pointer rather than copying
            // the string - this works because we keep the source code in memory.
            id[Name] = (int)pp;
            id[Hash] = tk;
            tk = id[Tk] = Id;
            return;
        }
        
        // Number parsing supports decimal, hex (0x), and octal (0) formats.
        // We don't handle floating point - that would add a lot of complexity
        // for a feature that isn't essential to demonstrate compiler concepts.
        else if (tk >= '0' && tk <= '9') {
            // Decimal numbers - straightforward base 10 accumulation
            if (ival = tk - '0') { 
                while (*p >= '0' && *p <= '9') 
                    ival = ival * 10 + *p++ - '0'; 
            }
            // Hex numbers - note the bit tricks to convert A-F to values
            else if (*p == 'x' || *p == 'X') {
                while ((tk = *++p) && ((tk >= '0' && tk <= '9') || 
                       (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
                    ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
            }
            // Octal numbers - base 8, indicated by leading 0
            else { 
                while (*p >= '0' && *p <= '7') 
                    ival = ival * 8 + *p++ - '0'; 
            }
            tk = Num;
            return;
        }
        
        // Comment handling is integrated with division operator recognition.
        // We only support C++ style // comments, not /* */ style comments.
        // This is another simplification that keeps the code cleaner while
        // still being useful in practice.
        else if (tk == '/') {
            if (*p == '/') {
                ++p;
                while (*p != 0 && *p != '\n') ++p;
            }
            else {
                tk = Div;
                return;
            }
        }
        
        // String and character literal handling supports basic escape sequences.
        // We store strings directly in the data segment and return a pointer.
        // This is simpler than managing a string table but means we can't
        // optimize identical strings.
        else if (tk == '\'' || tk == '"') {
            pp = data;  // Remember start of string in data segment
            while (*p != 0 && *p != tk) {
                if ((ival = *p++) == '\\') {
                    // Very basic escape handling - only \n for now
                    // A production compiler would support \r, \t, \", etc.
                    if ((ival = *p++) == 'n') ival = '\n';
                }
                if (tk == '"') *data++ = ival;
            }
            ++p;
            // For strings, return pointer to string start
            // For chars, return the character value
            if (tk == '"') ival = (int)pp; else tk = Num;
            return;
        }
        
        // Operator handling needs careful ordering to properly handle
        // multi-character operators like <= and ==. We check for the
        // longer versions first to avoid misinterpreting them as
        // single-character operators.
        else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
        else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
        else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
        else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
        else if (tk == '<') { 
            if (*p == '=') { ++p; tk = Le; } 
            else if (*p == '<') { ++p; tk = Shl; } 
            else tk = Lt; 
            return; 
        }
        else if (tk == '>') { 
            if (*p == '=') { ++p; tk = Ge; } 
            else if (*p == '>') { ++p; tk = Shr; } 
            else tk = Gt; 
            return; 
        }
        else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
        else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
        
        // Single-character operators and punctuation are the simplest case
        // We just return their token value directly with no extra processing
        else if (tk == '^') { tk = Xor; return; }
        else if (tk == '%') { tk = Mod; return; }
        else if (tk == '*') { tk = Mul; return; }
        else if (tk == '[') { tk = Brak; return; }
        else if (tk == '?') { tk = Cond; return; }
        // Punctuation characters are returned as-is - they don't need special tokens
        else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || 
                 tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
    }
}

// Expression parser using the precedence climbing algorithm
// This approach was chosen over recursive descent because it's more intuitive and requires 
// less code while handling operator precedence naturally. Each operator's precedence level
// determines when it gets evaluated, similar to how humans mentally parse expressions.
void expr(int lev)
{
    int t, *d;

    // Basic error checking and primary expression parsing
    // We need to validate the token stream before proceeding to avoid segfaults
    if (!tk) { 
        printf("%d: unexpected eof in expression\n", line); 
        exit(-1); 
    }
    else if (tk == Num) { 
        // Handle numeric literals by generating immediate load instructions
        // This creates a simple "push constant" operation in the VM
        *++e = IMM; 
        *++e = ival; 
        next(); 
        ty = INT; 
    }
    else if (tk == '"') {
        // String literals get stored in the data segment for efficiency
        // We handle consecutive strings (like "hello" "world") by combining them
        *++e = IMM; 
        *++e = ival; 
        next();
        while (tk == '"') next();
        // Align data pointer to word boundary to ensure efficient memory access
        // The bit mask trick (-sizeof(int) is all 1s except low bits) handles alignment
        data = (char *)((int)data + sizeof(int) & -sizeof(int)); 
        ty = PTR;
    }
    else if (tk == Sizeof) {
        // sizeof operator implementation - kept minimal but functional
        // We only support basic types (char/int) and pointers to reduce complexity
        // A full C compiler would need to handle structs, unions, etc.
        next();
        if (tk == '(') next(); else { 
            printf("%d: open paren expected in sizeof\n", line); 
            exit(-1); 
        }
        ty = INT;
        if (tk == Int) next();
        else if (tk == Char) { next(); ty = CHAR; }
        while (tk == Mul) { next(); ty = ty + PTR; }
        if (tk == ')') next(); else { 
            printf("%d: close paren expected in sizeof\n", line); 
            exit(-1); 
        }
        *++e = IMM;
        *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);
        ty = INT;
    }
    else if (tk == Id) {
        // Handle variables and function calls
        // The symbol table (id) stores all necessary type and scope information
        d = id; 
        next();
        if (tk == '(') {
            // Function call handling - we support both system calls and user functions
            // Arguments are pushed right-to-left to match C calling convention
            next();
            t = 0;
            while (tk != ')') { 
                expr(Assign); 
                *++e = PSH; 
                ++t; 
                if (tk == ',') next();
            }
            next();
            // Different handling for system calls vs user functions
            // System calls use direct execution while user functions need a call frame
            if (d[Class] == Sys) *++e = d[Val];
            else if (d[Class] == Fun) { 
                *++e = JSR; 
                *++e = d[Val]; 
            }
            else { 
                printf("%d: bad function call\n", line); 
                exit(-1); 
            }
            if (t) { 
                // Clean up stack after call - essential for maintaining stack balance
                *++e = ADJ; 
                *++e = t; 
            }
            ty = d[Type];
        }
        else if (d[Class] == Num) { 
            // Handle enum constants as immediate values
            *++e = IMM; 
            *++e = d[Val]; 
            ty = INT; 
        }
        else {
            // Variable access with proper scope handling
            // Locals use frame pointer offset, globals use absolute addresses
            if (d[Class] == Loc) { 
                *++e = LEA; 
                *++e = loc - d[Val]; 
            }
            else if (d[Class] == Glo) { 
                *++e = IMM; 
                *++e = d[Val]; 
            }
            else { 
                printf("%d: undefined variable\n", line); 
                exit(-1); 
            }
            // Load appropriate size - chars need special handling to only load a byte
            *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
        }
    }

    else if (tk == '(') {
        // Opening parenthesis could indicate either a type cast or a grouped expression
        // We peek at the next token to determine which case we're handling
        next();
        if (tk == Int || tk == Char) {
            // Type cast handling - C style casts like (int) or (char)
            // We track the target type and handle pointer types too
            t = (tk == Int) ? INT : CHAR;
            next();
            // Support pointer types in casts - (int*) or (char**)
            // Each * modifier increases the type level by PTR
            while (tk == Mul) { 
                next(); 
                t = t + PTR; 
            }
            if (tk == ')') next(); else { 
                printf("%d: bad cast\n", line); 
                exit(-1); 
            }
            // Parse the expression being cast with Inc precedence
            // Cast has higher precedence than most binary ops but lower than unary
            expr(Inc);
            ty = t;
        }
        else {
            // Regular parenthesized expression - maintains operator precedence
            // We use Assign precedence to allow any expression type inside parens
            expr(Assign);
            if (tk == ')') next(); else { 
                printf("%d: close paren expected\n", line); 
                exit(-1); 
            }
        }
    }
    else if (tk == Mul) {
        // Pointer dereference operator (*) - follows pointer to get value
        // Has high precedence to bind tightly to its operand
        next();
        expr(Inc);
        // Dereferencing decrements pointer level - *int** becomes int*
        // Only valid on pointer types (ty > INT)
        if (ty > INT) ty = ty - PTR; 
        else { 
            printf("%d: bad dereference\n", line); 
            exit(-1); 
        }
        // Generate load instruction based on pointed-to type
        *++e = (ty == CHAR) ? LC : LI;
    }
    else if (tk == And) {
        // Address-of operator (&) - gets pointer to a variable
        next();
        expr(Inc);
        // Clever optimization: if we're taking address of a loaded value,
        // we can just remove the load instruction and keep the address
        if (*e == LC || *e == LI) --e;
        else { 
            printf("%d: bad address-of\n", line); 
            exit(-1); 
        }
        // Taking address increases pointer level by PTR
        ty = ty + PTR;
    }
    else if (tk == '!') { 
        // Logical NOT - converts value to boolean and inverts it
        // Implemented by comparing with zero (true if equal to zero)
        next(); 
        expr(Inc);
        *++e = PSH; 
        *++e = IMM; 
        *++e = 0; 
        *++e = EQ;
        ty = INT; 
    }
    else if (tk == '~') { 
        // Bitwise NOT - inverts all bits using XOR with -1
        // -1 in two's complement has all bits set to 1
        next(); 
        expr(Inc);
        *++e = PSH; 
        *++e = IMM; 
        *++e = -1; 
        *++e = XOR;
        ty = INT; 
    }
    else if (tk == Add) {
        // Unary plus - included for completeness but doesn't change value
        // Most C compilers optimize this away entirely
        next(); 
        expr(Inc); 
        ty = INT;
    }
    else if (tk == Sub) {
        // Unary minus - negates a value
        // Special case for constants to avoid runtime calculation
        next();
        *++e = IMM;
        if (tk == Num) {
            // For numeric constants, just negate the value directly
            *++e = -ival; 
            next();
        }
        else {
            // For expressions, multiply by -1 at runtime
            *++e = -1;
            *++e = PSH;
            expr(Inc);
            *++e = MUL;
        }
        ty = INT;
    }
    else if (tk == Inc || tk == Dec) {
        // Pre-increment/decrement (++x or --x)
        // More complex than post operations since we need to return new value
        t = tk;
        next();
        expr(Inc);
        // Ensure we're modifying an l-value (something that can be assigned to)
        if (*e == LC) { *e = PSH; *++e = LC; }
        else if (*e == LI) { *e = PSH; *++e = LI; }
        else { 
            printf("%d: bad lvalue in pre-increment\n", line); 
            exit(-1); 
        }
        // Generate code to modify value in place
        // For pointers, we adjust by sizeof(type) instead of 1
        *++e = PSH;
        *++e = IMM;
        *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
        *++e = (t == Inc) ? ADD : SUB;
        *++e = (ty == CHAR) ? SC : SI;
    }
    else { 
        printf("%d: bad expression\n", line); 
        exit(-1); 
    }

    // Core of the precedence climbing algorithm
    // We keep processing operators as long as they have precedence >= current level
    // This naturally builds the operator tree with correct associativity
    while (tk >= lev) {
        // Cache the type for pointer operations and assignments
        // Essential for handling complex expressions with mixed types
        t = ty;
        if (tk == Assign) {
            // Assignment requires an l-value (addressable location) on the left
            // We verify this by checking if the last instruction was a load
            next();
            if (*e == LC || *e == LI) *e = PSH; 
            else { 
                printf("%d: bad lvalue in assignment\n", line); 
                exit(-1); 
            }
            expr(Assign);
            // Store with appropriate size based on target type
            *++e = ((ty = t) == CHAR) ? SC : SI;
        }
        else if (tk == Cond) {
            // Ternary operator (?:) - implements conditional branching
            // Generates code similar to: condition ? true_expr : false_expr
            next();
            *++e = BZ;  // Branch if zero (false)
            d = ++e;    // Save location for backpatching
            expr(Assign);
            if (tk == ':') next(); 
            else { 
                printf("%d: conditional missing colon\n", line); 
                exit(-1); 
            }
            *d = (int)(e + 3);        // Jump past the true expression if condition is false
            *++e = JMP;               // Unconditional jump after true expression
            d = ++e;                  // Save location for backpatching
            expr(Cond);               // Parse false expression
            *d = (int)(e + 1);        // Jump to end after false expression
        }
        else if (tk == Lor) { 
            // Logical OR with short-circuit evaluation
            // If first operand is true, skip evaluating second operand
            next(); 
            *++e = BNZ;  // Branch if non-zero (true)
            d = ++e;     // Save location for backpatching
            expr(Lan);   // Parse right operand
            *d = (int)(e + 1); 
            ty = INT; 
        }
        else if (tk == Lan) { 
            // Logical AND with short-circuit evaluation
            // If first operand is false, skip evaluating second operand
            next(); 
            *++e = BZ;   // Branch if zero (false)
            d = ++e;     // Save location for backpatching
            expr(Or);    // Parse right operand
            *d = (int)(e + 1); 
            ty = INT; 
        }
        // Binary operators follow a consistent pattern:
        // 1. Push left operand
        // 2. Evaluate right operand with appropriate precedence
        // 3. Generate operation instruction
        // 4. Result type is always INT for logical/bitwise ops
        else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
        else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
        else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
        else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
        else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
        else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
        else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
        else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
        else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
        else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
        else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
        else if (tk == Add) {
            // Addition with pointer arithmetic support
            // When adding to a pointer, we scale the right operand
            next(); 
            *++e = PSH; 
            expr(Mul);
            if ((ty = t) > PTR) { 
                // Scale the index by the size of the pointed-to type
                // This ensures ptr + 1 moves to the next element
                *++e = PSH; 
                *++e = IMM; 
                *++e = sizeof(int); 
                *++e = MUL;
            }
            *++e = ADD;
        }
        else if (tk == Sub) {
            // Subtraction handles both pointer arithmetic and pointer differences
            next(); 
            *++e = PSH; 
            expr(Mul);
            if (t > PTR && t == ty) { 
                // Pointer subtraction: returns number of elements between pointers
                // Result needs to be divided by element size
                *++e = SUB; 
                *++e = PSH; 
                *++e = IMM; 
                *++e = sizeof(int); 
                *++e = DIV; 
                ty = INT;
            }
            else if ((ty = t) > PTR) { 
                // Pointer arithmetic: similar to addition but subtracts
                *++e = PSH; 
                *++e = IMM; 
                *++e = sizeof(int); 
                *++e = MUL; 
                *++e = SUB;
            }
            else *++e = SUB;
        }
        else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
        else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
        else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
        else if (tk == Inc || tk == Dec) {
            // Post-increment/decrement (x++ or x--)
            // More complex than pre-increment because we need to:
            // 1. Save the original value
            // 2. Increment/decrement the storage location
            // 3. Return the original value
            if (*e == LC) { *e = PSH; *++e = LC; }
            else if (*e == LI) { *e = PSH; *++e = LI; }
            else { 
                printf("%d: bad lvalue in post-increment\n", line); 
                exit(-1); 
            }
            *++e = PSH; 
            *++e = IMM; 
            *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
            *++e = (tk == Inc) ? ADD : SUB;
            *++e = (ty == CHAR) ? SC : SI;
            *++e = PSH; 
            *++e = IMM; 
            *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
            *++e = (tk == Inc) ? SUB : ADD;
            next();
        }
        else if (tk == Brak) {
            // Array subscripting - converts a[b] into *(a + b)
            // Handles both array indexing and pointer arithmetic
            next(); 
            *++e = PSH; 
            expr(Assign);
            if (tk == ']') next(); 
            else { 
                printf("%d: close bracket expected\n", line); 
                exit(-1); 
            }
            if (t > PTR) { 
                // Scale the index based on the pointed-to type size
                // This ensures proper element addressing
                *++e = PSH; 
                *++e = IMM; 
                *++e = sizeof(int); 
                *++e = MUL;
            }
            else if (t < PTR) { 
                printf("%d: pointer type expected\n", line); 
                exit(-1); 
            }
            *++e = ADD;
            // Final load of the computed address
            *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
        }
        else { 
            printf("%d: compiler error tk=%d\n", line, tk); 
        }
    }
}

// Statement parser - the heart of control flow implementation
// Handles all C control structures, compound blocks, and expression statements
// Uses a recursive descent approach with backpatching for control flow
void stmt()
{
    int *a, *b;  // Used for tracking jump addresses in control structures

    if (tk == If) {
        // If-else implementation using conditional jumps and backpatching
        // The tricky part is handling the optional else clause correctly
        next();
        if (tk == '(') next(); else { 
            printf("%d: open paren expected\n", line); 
            exit(-1); 
        }
        // Parse condition - needs Assign precedence to allow complex expressions
        // like if (x = y) which are valid in C
        expr(Assign);
        if (tk == ')') next(); else { 
            printf("%d: close paren expected\n", line); 
            exit(-1); 
        }
        
        // Generate code for conditional jump
        // BZ (Branch if Zero) skips the 'then' block if condition is false
        *++e = BZ; 
        b = ++e;  // Save location to backpatch once we know where to jump
        
        stmt();  // Recursively parse the 'then' statement
        if (tk == Else) {
            // Handling else requires an extra jump to skip the else block
            // when the 'then' block executes
            *b = (int)(e + 3);  // Jump to instruction after JMP
            *++e = JMP;         // Unconditional jump over else block
            b = ++e;            // Save location for backpatching
            next();
            stmt();  // Recursively parse the 'else' statement
        }
        // Backpatch the jump address - either skips 'then' or 'else' block
        *b = (int)(e + 1);
    }
    else if (tk == While) {
        // While loop implementation using backward jump
        // We need two jumps: conditional forward jump to exit
        // and unconditional backward jump to loop
        next();
        a = e + 1;  // Save starting point for backward jump (condition check)
        if (tk == '(') next(); else { 
            printf("%d: open paren expected\n", line); 
            exit(-1); 
        }
        expr(Assign);  // Parse loop condition
        if (tk == ')') next(); else { 
            printf("%d: close paren expected\n", line); 
            exit(-1); 
        }
        
        // Generate conditional exit jump
        *++e = BZ; 
        b = ++e;  // Save location to backpatch exit point
        
        stmt();  // Parse loop body
        
        // Create the actual loop
        *++e = JMP;          // Unconditional jump back to condition
        *++e = (int)a;       // Target is start of condition
        *b = (int)(e + 1);   // Backpatch exit jump to skip the loop
    }
    else if (tk == Return) {
        // Return statement handling
        // Supports both void return and return with value
        next();
        if (tk != ';') expr(Assign);  // Parse return value if present
        *++e = LEV;  // LEV instruction restores previous frame and returns
        if (tk == ';') next(); else { 
            printf("%d: semicolon expected\n", line); 
            exit(-1); 
        }
    }
    else if (tk == '{') {
        // Compound statement - creates a new scope
        // Recursively parses statements until closing brace
        // This enables nested blocks and local variable scoping
        next();
        while (tk != '}') stmt();
        next();
    }
    else if (tk == ';') {
        // Empty statement - useful in loops like: while (x) ;
        // Also helps with error recovery in some cases
        next();
    }
    else {
        // Expression statement - common for function calls, assignments
        // Side effects from the expression are the purpose here
        expr(Assign);
        if (tk == ';') next(); else { 
            printf("%d: semicolon expected\n", line); 
            exit(-1); 
        }
    }
}

int main(int argc, char **argv)
{
    // Core VM registers and control variables
    // These form the heart of our virtual machine implementation:
    // - pc: Program counter for tracking execution position
    // - sp: Stack pointer managing our call stack
    // - bp: Base pointer for function frame management
    // It uses a simple register-based VM design rather than a stack-based one
    // because it maps more naturally to C's execution model and makes
    // code generation more straightforward
    int fd, bt, ty, poolsz, *idmain;
    int *pc, *sp, *bp, a, cycle;
    int i, *t;

    // Command line processing
    // It supports two debug flags that are crucial for compiler development:
    // -s: Shows source alongside assembly (great for understanding code gen)
    // -d: Displays executed instructions (helps trace runtime behavior)
    // These were chosen as minimal but effective debugging tools based on
    // experience with what's most useful when working on the compiler
    --argc; ++argv;
    if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { 
        src = 1;   
        --argc; ++argv; 
    }
    if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { 
        debug = 1;  
        --argc; ++argv; 
    }
    if (argc < 1) { 
        printf("usage: c4 [-s] [-d] file ...\n"); 
        return -1; 
    }

    // Source file handling
    // It uses raw file I/O instead of stdio's buffered I/O for simplicity.
    // While this might be less efficient for large files, it keeps our
    // I/O code minimal and avoids stdio complexity
    if ((fd = open(*argv, 0)) < 0) { 
        printf("could not open(%s)\n", *argv); 
        return -1; 
    }

    // Memory management
    // It uses fixed-size pools for all compiler data structures. 256KB was
    // chosen as a balance between supporting reasonable program sizes while
    // keeping memory management dead simple. This avoids the complexity of
    // dynamic resizing but means we have hard limits on program size.
     
    // It's been allocated four distinct pools:
    // - sym: Symbol table (identifiers, functions, variables)
    // - e: Text segment (generated code)
    // - data: Data segment (global variables)
    // - sp: Runtime stack
     
    // This separation mirrors a real program's memory layout and makes
    // memory management more predictable
    poolsz = 256*1024;  
    if (!(sym = malloc(poolsz))) { 
        printf("could not malloc(%d) symbol area\n", poolsz); 
        return -1; 
    }
    if (!(le = e = malloc(poolsz))) { 
        printf("could not malloc(%d) text area\n", poolsz); 
        return -1; 
    }
    if (!(data = malloc(poolsz))) { 
        printf("could not malloc(%d) data area\n", poolsz); 
        return -1; 
    }
    if (!(sp = malloc(poolsz))) { 
        printf("could not malloc(%d) stack area\n", poolsz); 
        return -1; 
    }

    // Initialize memory pools to zero
    // This ensures predictable behavior - uninitialized variables will
    // have a known value and makes debugging easier
    memset(sym,  0, poolsz);
    memset(e,    0, poolsz);
    memset(data, 0, poolsz);

    // Keyword initialization
    // Populates the symbol table with C keywords and basic types.
    // The order here matters - it matches the token enum values defined
    // earlier, allowing us to use simple integer comparison for keyword
    // recognition during lexing
    p = "char else enum if int return sizeof while "
        "open read close printf malloc free memset memcmp exit void main";
    i = Char; 
    while (i <= While) { 
        next(); 
        id[Tk] = i++; 
    }

    // System call setup
    // Adds a basic I/O and memory management functions to the symbol table.
    // These are treated specially during code generation - they map directly
    // to system calls rather than generating normal function calls.
    // This gives us essential functionality without having to implement
    // a standard library
    i = OPEN;
    while (i <= EXIT) { 
        next(); 
        id[Class] = Sys; 
        id[Type] = INT; 
        id[Val] = i++; 
    }
    
    // Special handling for void and main
    // Tracks main() specially since it's our entry point.
    // void is handled as a special type even though we don't fully
    // support type checking
    next(); id[Tk] = Char;  
    next(); idmain = id;    

    // Source code loading
    // Reads the entire source into memory at once for simplicity.
    // This limits program size but makes lexing much simpler since we
    // can treat the source as a string
    if (!(lp = p = malloc(poolsz))) { 
        printf("could not malloc(%d) source area\n", poolsz); 
        return -1; 
    }
    if ((i = read(fd, p, poolsz-1)) <= 0) { 
        printf("read() returned %d\n", i); 
        return -1; 
    }
    p[i] = 0;  
    close(fd);

    // Parse declarations
    // This is the main parsing loop for global scope declarations.
    // It handles three types of declarations:
    // 1. Basic types (int, char)
    // 2. Enums (with optional value assignments)
    // 3. Function definitions and global variables
     
    // The parser is recursive descent, chosen for its simplicity and
    // direct mapping to C's grammar. While not the most efficient,
    // it's easy to understand and maintain
    line = 1;
    next();
    while (tk) {
        bt = INT;  
        if (tk == Int) next();
        else if (tk == Char) { next(); bt = CHAR; }
        else if (tk == Enum) {
            // Enum handling
            // Treats enums as simple integer constants. Values auto-increment
            // unless explicitly assigned. This matches C semantics while
            // keeping the implementation simple
            next();
            if (tk != '{') next();
            if (tk == '{') {
                next();
                i = 0;  
                while (tk != '}') {
                    if (tk != Id) { 
                        printf("%d: bad enum identifier %d\n", line, tk); 
                        return -1; 
                    }
                    next();
                    if (tk == Assign) {
                        next();
                        if (tk != Num) { 
                            printf("%d: bad enum initializer\n", line); 
                            return -1; 
                        }
                        i = ival;
                        next();
                    }
                    id[Class] = Num;
                    id[Type] = INT;
                    id[Val] = i++;
                    if (tk == ',') next();
                }
                next();
            }
        }

        // Declaration parsing
        // This handles both function and variable declarations.
        // It supports basic pointer types through the Mul token (*).
        // The symbol table tracks everything's type and scope for
        // later use during code generation
        while (tk != ';' && tk != '}') {
            ty = bt;  
            while (tk == Mul) { next(); ty = ty + PTR; }  
            if (tk != Id) { 
                printf("%d: bad global declaration\n", line); 
                return -1; 
            }
            if (id[Class]) { 
                printf("%d: duplicate global definition\n", line); 
                return -1; 
            }
            next();
            id[Type] = ty;
            if (tk == '(') {  
                // Function declaration handling
                // It saves the current position in the code segment (e)
                // as the function's entry point. Parameters are treated
                // as local variables with positive offsets from bp.
                 
                // The HClass/HType/HVal fields save the global scope
                // information while we process the function body, allowing
                // us to restore it later
                id[Class] = Fun;
                id[Val] = (int)(e + 1);
                next();
                i = 0;  
                while (tk != ')') {
                    ty = INT;
                    if (tk == Int) next();
                    else if (tk == Char) { next(); ty = CHAR; }
                    while (tk == Mul) { next(); ty = ty + PTR; }
                    if (tk != Id) { 
                        printf("%d: bad parameter declaration\n", line); 
                        return -1; 
                    }
                    if (id[Class] == Loc) { 
                        printf("%d: duplicate parameter definition\n", line); 
                        return -1; 
                    }
                    id[HClass] = id[Class]; 
                    id[Class] = Loc;
                    id[HType] = id[Type];   
                    id[Type] = ty;
                    id[HVal] = id[Val];     
                    id[Val] = i++;
                    next();
                    if (tk == ',') next();
                }
                next();
                if (tk != '{') { 
                    printf("%d: bad function definition\n", line); 
                    return -1; 
                }
                loc = ++i;  
                next();
                // Local variable handling
                // Similar to parameters but with negative offsets from bp.
                // Tracks the total size to allocate the correct stack
                // frame size during function entry
                while (tk == Int || tk == Char) {
                    bt = (tk == Int) ? INT : CHAR;
                    next();
                    while (tk != ';') {
                        ty = bt;
                        while (tk == Mul) { next(); ty = ty + PTR; }
                        if (tk != Id) { 
                            printf("%d: bad local declaration\n", line); 
                            return -1; 
                        }
                        if (id[Class] == Loc) { 
                            printf("%d: duplicate local definition\n", line); 
                            return -1; 
                        }
                        id[HClass] = id[Class]; 
                        id[Class] = Loc;
                        id[HType] = id[Type];  
                        id[Type] = ty;
                        id[HVal] = id[Val];    
                        id[Val] = ++i;
                        next();
                        if (tk == ',') next();
                    }
                    next();
                }
                // Function body generation
                // ENT sets up the stack frame, then we compile the body,
                // and LEV tears down the frame and returns.
                // This gives us proper function call semantics with
                // local variable support
                *++e = ENT; 
                *++e = i - loc;
                while (tk != '}') stmt();
                *++e = LEV;
                // Scope cleanup
                // Restore the global symbol table state that we saved
                // earlier. This lets us handle nested functions if we
                // want to support them later
                id = sym; 
                while (id[Tk]) {
                    if (id[Class] == Loc) {
                        id[Class] = id[HClass];
                        id[Type] = id[HType];
                        id[Val] = id[HVal];
                    }
                    id = id + Idsz;
                }
            }
            else {  
                // Global variable allocation
                // Globals are simply stored in the data segment
                // with static addresses. We don't support initialization
                // yet - all globals start as 0
                id[Class] = Glo;
                id[Val] = (int)data;
                data = data + sizeof(int);
            }
            if (tk == ',') next();
        }
        next();
    }

    // Setup for execution
    if (!(pc = (int *)idmain[Val])) { 
        printf("main() not defined\n"); 
        return -1; 
    }
    if (src) return 0;  // Skip execution if just showing assembly

    // Setup stack
    bp = sp = (int *)((int)sp + poolsz);
    *--sp = EXIT;  // Call exit if main returns
    *--sp = PSH; 
    t = sp;
    *--sp = argc;
    *--sp = (int)argv;
    *--sp = (int)t;

    // Run the virtual machine
    cycle = 0;  // Instruction cycle counter - tracks number of instructions executed
    
    while (1) {  // Main execution loop - runs until EXIT instruction or error
        i = *pc++;  // Fetch next instruction and increment program counter
        ++cycle;
        
        // Debug output - prints human-readable instruction names
        if (debug) {
            // Clever use of string indexing to lookup instruction names
            // Each instruction name is exactly 5 chars (including spaces)
            printf("%d> %.4s", cycle,
                &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
            
            // Print operand for instructions that use immediate values
            if (i <= ADJ) printf(" %d\n", *pc); 
            else printf("\n");
        }

        // Main instruction decoder and executor
        // Memory Access Instructions
        if (i == LEA) {
            // Load Effective Address: Calculate address relative to base pointer
            // Used for accessing local variables and parameters
            a = (int)(bp + *pc++);
        }
        else if (i == IMM) {
            // Load Immediate: Load constant value directly into accumulator
            a = *pc++;
        }
        else if (i == LI) {
            // Load Int: Dereference address in accumulator to load integer value
            a = *(int *)a;
        }
        else if (i == LC) {
            // Load Char: Dereference address in accumulator to load character
            a = *(char *)a;
        }
        else if (i == SI) {
            // Store Int: Store accumulator value at address popped from stack
            *(int *)*sp++ = a;
        }
        else if (i == SC) {
            // Store Char: Store lower byte of accumulator at address from stack
            a = *(char *)*sp++ = a;
        }
        
        // Control Flow Instructions
        else if (i == JMP) {
            // Unconditional Jump: Set program counter to target address
            pc = (int *)*pc;
        }
        else if (i == JSR) {
            // Jump to Subroutine: Save return address and jump to target
            *--sp = (int)(pc + 1);  // Push return address
            pc = (int *)*pc;        // Jump to subroutine
        }
        else if (i == BZ) {
            // Branch if Zero: Conditional jump based on accumulator value
            pc = a ? pc + 1 : (int *)*pc;
        }
        else if (i == BNZ) {
            // Branch if Not Zero: Opposite of BZ
            pc = a ? (int *)*pc : pc + 1;
        }
        
        // Function Call Management
        else if (i == ENT) {
            // Enter Function: Set up new stack frame for function
            *--sp = (int)bp;     // Save old base pointer
            bp = sp;             // Set new base pointer
            sp = sp - *pc++;     // Allocate space for local variables
        }
        else if (i == ADJ) {
            // Adjust Stack: Clean up parameters after function call
            sp = sp + *pc++;
        }
        else if (i == LEV) {
            // Leave Function: Restore stack frame and return
            sp = bp;           // Restore stack pointer
            bp = (int *)*sp++; // Restore base pointer
            pc = (int *)*sp++; // Load return address
        }
        else if (i == PSH) {
            // Push: Push accumulator value onto stack
            *--sp = a;
        }
        
        // Arithmetic and Logic Operations
        else if (i == OR)  a = *sp++ |  a;   // Bitwise OR
        else if (i == XOR) a = *sp++ ^  a;   // Bitwise XOR
        else if (i == AND) a = *sp++ &  a;   // Bitwise AND
        else if (i == EQ)  a = *sp++ == a;   // Equal comparison
        else if (i == NE)  a = *sp++ != a;   // Not equal comparison
        else if (i == LT)  a = *sp++ <  a;   // Less than comparison
        else if (i == GT)  a = *sp++ >  a;   // Greater than comparison
        else if (i == LE)  a = *sp++ <= a;   // Less than or equal
        else if (i == GE)  a = *sp++ >= a;   // Greater than or equal
        else if (i == SHL) a = *sp++ << a;   // Shift left
        else if (i == SHR) a = *sp++ >> a;   // Shift right
        else if (i == ADD) a = *sp++ +  a;   // Addition
        else if (i == SUB) a = *sp++ -  a;   // Subtraction
        else if (i == MUL) a = *sp++ *  a;   // Multiplication
        else if (i == DIV) a = *sp++ /  a;   // Division
        else if (i == MOD) a = *sp++ %  a;   // Modulo
        
        // System Calls
        else if (i == OPEN) {
            // File open syscall wrapper
            a = open((char *)sp[1], *sp);
        }
        else if (i == READ) {
            // File read syscall wrapper
            a = read(sp[2], (char *)sp[1], *sp);
        }
        else if (i == CLOS) {
            // File close syscall wrapper
            a = close(*sp);
        }
        else if (i == PRTF) {
            // Printf wrapper - supports up to 6 arguments
            t = sp + pc[1];  // Point to format string and arguments
            a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]);
        }
        
        // Memory Management
        else if (i == MALC) {
            // Memory allocation wrapper
            a = (int)malloc(*sp);
        }
        else if (i == FREE) {
            // Memory deallocation
            free((void *)*sp);
        }
        else if (i == MSET) {
            // Memory set operation
            a = (int)memset((char *)sp[2], sp[1], *sp);
        }
        else if (i == MCMP) {
            // Memory compare operation
            a = memcmp((char *)sp[2], (char *)sp[1], *sp);
        }
        
        // VM Control
        else if (i == EXIT) {
            // Exit VM with return value from stack
            printf("exit(%d) cycle = %d\n", *sp, cycle);
            return *sp;
        }
        else {
            // Error handling for invalid instructions
            printf("unknown instruction = %d! cycle = %d\n", i, cycle);
            return -1;
        }
    }
}
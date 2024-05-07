# C++ Compiler
Compiles C++(subset of functionalities) down to C(subset of functionalities) and evaluates it. 

### Run instructions
- OCAML version >5.1
- Use `make` to compile the code. Executable with name `cpp_compiler` is created
- Testing: `./cpp_compiler < test/100_shared_ptr_basic.cppish`
- To run multiple tests from test folder in the project `./runit.sh`. This will create a output directory with their corresponding input filenames. 
- (For debugging) To view the generated Cish code set `print_cish_ast` to true in cppish_compile.ml

For detailed design details, refer to [this](CompilersProjectReport.pdf) report
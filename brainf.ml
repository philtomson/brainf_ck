(* BrainF*ck implemented as a direct-threaded interpreter *)

 let each_char fn str = 
    let len = String.length str in
    let rec iter idx = if idx < len then
                         (fn (str.[idx]) idx ; iter (idx + 1)) 
                       else
                         ()  in
    iter 0 ;;


let code_size = 1024*32;;
let put_char c = print_char(char_of_int c);;
let get_char () = int_of_char(input_char stdin);;

(* create opcode array & fill with dummy function that takes two ints *)
let op_array = Array.create code_size (fun (x:int) (y:int) -> Printf.printf
"\nDone.\n" ) ;;
let data = Array.init code_size ( fun _ -> 0) ;;
let stack   = Stack.create () ;;
let jmp_table = Array.init code_size ( fun _ -> 0) ;;

(* direct threading opcodes *)

let op_plus pc data_i   =  data.(data_i) <- data.(data_i) + 1;
                           (op_array.(pc+1)) (pc+1) (data_i) ;;

let op_minus pc data_i   =  data.(data_i) <- data.(data_i) - 1;
                           (op_array.(pc+1)) (pc+1) (data_i) ;;

let op_right pc data_i  =  (op_array.(pc+1)) (pc+1) (data_i+1);;

let op_left pc data_i   =  (op_array.(pc+1)) (pc+1) (data_i-1);;

let op_put  pc data_i   =  
                           put_char(data.(data_i));
                           (op_array.(pc+1)) (pc+1) data_i;;

let op_get  pc data_i   =  data.(data_i) <- get_char ();
                           (op_array.(pc+1)) (pc+1) data_i;;

let op_if  pc data_i    =  let new_pc = 
                             if data.(data_i) = 0 then 
                               jmp_table.(pc) + 1
                             else
                               pc + 1 in
                          (op_array.(new_pc)) new_pc data_i ;;

let op_back pc data_i   =  let new_pc = jmp_table.(pc) in
                           (op_array.(new_pc)) new_pc data_i ;;

let op_end pc data_i = Printf.printf "\nDone\n";;

let build_program code  =  
  each_char (
    fun c offset -> (*Printf.printf "%c : %d" c offset;*)
    match c with
    | '>' -> op_array.(offset) <- op_right (*Printf.printf " Right\n"*)
    | '<' -> op_array.(offset) <- op_left  (*Printf.printf " Left\n"*)
    | '+' -> op_array.(offset) <- op_plus  (*Printf.printf " Plus\n"*)
    | '-' -> op_array.(offset) <- op_minus (*Printf.printf " Minus\n"*)
    | '.' -> op_array.(offset) <- op_put   (*Printf.printf " Put\n"*)
    | ',' -> op_array.(offset) <- op_get   (*Printf.printf " Get\n"*)
    | '[' -> Stack.push offset stack;       
             op_array.(offset) <- op_if (*Printf.printf " If(\n"     *)
    | ']' -> jmp_table.((Stack.top stack)) <- offset;
             jmp_table.(offset) <- Stack.pop stack;
             op_array.(offset) <- op_back (*; Printf.printf " )\n" *)
    | _   -> () (*skip everything else*) 
  ) code;;


let _ = 
  (* print Hello World! *)
  build_program "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.";
  (* run program *)
  op_array.(0) 0 0 ;;

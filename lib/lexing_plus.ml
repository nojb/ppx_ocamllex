let init_lexbuf lexbuf mem_size =
  let pos = lexbuf.Lexing.lex_curr_pos in
  lexbuf.Lexing.lex_mem <- Array.make mem_size (-1);
  lexbuf.Lexing.lex_start_pos <- pos;
  lexbuf.Lexing.lex_last_pos <- pos;
  lexbuf.Lexing.lex_last_action <- -1

let rec next_char lexbuf =
  if lexbuf.Lexing.lex_curr_pos >= lexbuf.Lexing.lex_buffer_len then begin
    if lexbuf.Lexing.lex_eof_reached then
      256
    else begin
      lexbuf.Lexing.refill_buff lexbuf;
      next_char lexbuf
    end
  end else begin
    let i = lexbuf.Lexing.lex_curr_pos in
    let c = lexbuf.Lexing.lex_buffer.[i] in
    lexbuf.Lexing.lex_curr_pos <- i+1;
    Char.code c
  end

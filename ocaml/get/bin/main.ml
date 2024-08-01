let sock = Unix.socket Unix.PF_INET SOCK_STREAM 0
let inet_addr = Unix.inet_addr_of_string "127.0.0.1"

let () =
  let _ = Unix.connect sock (ADDR_INET (inet_addr, 8000)) in
  let _ = Unix.send sock (Bytes.of_string "GET /README.md HTTP/1.0\n\n") 0 25 [] in
  let buffer = Bytes.create 512 in
  let rec read sock =
    match Unix.recv sock buffer 0 1 [] with
    | 0 -> ()
    | n ->
      let _ = Unix.write Unix.stdout buffer 0 n in
      read sock
  in
  read sock
;;

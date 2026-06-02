let add_remove_pat = CCOption.get_exn_or "add_remove_pat" (Lua_pattern.of_string "^( +)([+~-])")
let add_remove_sub = Lua_pattern.rep_str "%2%1"

(* Promote a leading [+]/[~]/[-] marker to column 0 so GitHub's diff-highlighted
   code fence colours the line, and turn a leading [~] (a change) into [!]. *)
let transform_line line =
  let line =
    match Lua_pattern.substitute ~s:line ~r:add_remove_sub add_remove_pat with
    | Some line -> line
    | None -> line
  in
  if (not (CCString.is_empty line)) && line.[0] = '~' then CCString.set line 0 '!' else line

let is_ident_start c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_'
let is_ident c = is_ident_start c || (c >= '0' && c <= '9')

(* If [line] opens a Terraform heredoc string value, return its delimiter. The
   delimiter is an arbitrary HCL identifier ([EOT] is just the conventional
   one), so we parse it directly rather than hardcode a name: [<<], an optional
   [-] (the indented [<<-] form), an optional quote, then the identifier. *)
let heredoc_delim line =
  match CCString.find ~sub:"<<" line with
  | -1 -> None
  | i ->
      let len = String.length line in
      let pos = i + 2 in
      let pos = if pos < len && line.[pos] = '-' then pos + 1 else pos in
      let pos = if pos < len && line.[pos] = '"' then pos + 1 else pos in
      if pos < len && is_ident_start line.[pos] then (
        let stop = ref pos in
        while !stop < len && is_ident line.[!stop] do
          incr stop
        done;
        Some (String.sub line pos (!stop - pos)))
      else None

let transform plan_text =
  plan_text
  |> CCString.split_on_char '\n'
  |> CCList.fold_left
       (fun (delim, acc) line ->
         match delim with
         | Some d ->
             (* Inside a heredoc body the content is an opaque string value
                (often YAML, whose [- ] list items look exactly like removal
                markers), so emit it verbatim. Leave the body when we reach the
                closing delimiter line. *)
             let delim = if CCString.equal (CCString.trim line) d then None else delim in
             (delim, line :: acc)
         | None ->
             (* The opening line itself (e.g. [~ values = <<-EOT]) is a real
                diff line, so transform it before entering the body. *)
             (heredoc_delim line, transform_line line :: acc))
       (None, [])
  |> snd
  |> CCList.rev
  |> CCString.concat "\n"

open Core.Std
open Async.Std
open Cohttp_async

module Html = struct
  type page = {
    directory: string;
    (* relative paths from directory *)
    files : string list;
    directories : string list;
  }
  let link ~href ~text = sprintf "<a href='%s'>%s</a>" href text
  let page ~title ~body = sprintf " <html>
              <head><title>%s</title>
              <body> %s </body>
              </html>"
      title body
  let li x = "<li>" ^ x ^ "</li>"
  let ul x = "<ul>\n" ^ x ^ "\n</ul>"
  let render_page {directory; files; directories} = 
    let full_path f = "/" ^ (Filename.concat directory f) in
    let listify paths = 
      List.map ~f:(fun f -> link ~href:(full_path f) ~text:f |> li) paths
      |> String.concat ~sep:"\n" |> ul in
    let body =
      (listify directories) ^ "\n<hr>\n" ^ (listify files)
    in page ~title:directory ~body
end

let page_of_directory directory = 
  let open Core.Std in (* TODO : blocking calls *)
  let (directories, files) = 
    directory 
    |> Sys.ls_dir
    |> List.filter ~f:(fun f -> 
        match Sys.is_file (Filename.concat directory f) with
        | `Unknown -> (printf "Unsure what do about %s. Skipping" f; false)
        | _ -> true)
    |> List.partition_tf ~f:(fun f ->
        match Sys.is_file (Filename.concat directory f) with
        | `Unknown -> assert false
        | `Yes -> false
        | `No -> true)
  in Html.({ directory; directories; files })

let no_slash u = String.strip ~drop:(fun c -> c = '/') u

let valid_path ~root path =
  try 
    let (root, path) = Filename.(realpath root, realpath path) in
    let rec loop = function
      | r::rs, p::ps when r = p -> loop (rs, ps)
      | [], _::_ -> true
      | _, _ -> false
    in loop Filename.(parts root, parts path)
  with Unix.Unix_error (_, _, _) -> 
    if path <> "favicon.ico" then
      printf "Could not find %s in %s\n" path root; false

let not_found path =
  Server.respond_with_string ~code:`Not_found ("Not found: " ^ path)

let handler ~root ~body:_ sock req = 
  let uri = Cohttp.Request.uri req in
  let render_dir p = 
    p |> page_of_directory |> Html.render_page |> Server.respond_with_string
  in
  match uri |> Uri.path |> no_slash with
  | "" -> render_dir root
  | path when valid_path ~root path ->
    let fullpath = Filename.concat root path in
    let open Core.Std in (* TODO : blocking calls *)
    begin match Sys.is_file fullpath with
      | `Unknown -> not_found path
      | `Yes -> fullpath |> Server.respond_with_file
      | `No -> render_dir fullpath
    end 
  | path -> not_found path

let command =
  Command.async_basic
    ~summary:"Serve a directory's contents using cohttp"
    Command.Spec.(
      empty
      +> flag "-port" (optional_with_default 8080 int)
          ~doc:"Port of http server default is 8080"
      +> anon (maybe_with_default "." ("<directory to serve>" %: string))
    )
    (fun port root () -> 
       printf "Serving '%s' on port %d\n" root port;
       (Server.create ~on_handler_error:`Ignore
          (Tcp.on_port port) (handler ~root)) >>= fun _ ->
       Deferred.never () )

let () = Command.run command

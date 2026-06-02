let test_simple =
  Oth.test ~name:"Test simple" (fun _ ->
      let plan_text =
        "Terraform used the selected providers to generate the following execution\n\
         plan. Resource actions are indicated with the following symbols:\n\
        \  + create\n\n\
         Terraform will perform the following actions:\n\n\
        \  # null_resource.bar will be created\n\
        \  + resource \"null_resource\" \"bar\" {\n\
        \      + id = (known after apply)\n\
        \    }\n\n\
        \  # null_resource.baz will be created\n\
        \  + resource \"null_resource\" \"baz\" {\n\
        \      + id = (known after apply)\n\
        \    }\n\n\
        \  # null_resource.foo will be created\n\
        \  ~ resource \"null_resource\" \"foo\" {\n\
        \      - id = (known after apply)\n\
        \    }\n\n\
         Plan: 3 to add, 0 to change, 0 to destroy."
      in
      let plan_diff =
        "Terraform used the selected providers to generate the following execution\n\
         plan. Resource actions are indicated with the following symbols:\n\
         +   create\n\n\
         Terraform will perform the following actions:\n\n\
        \  # null_resource.bar will be created\n\
         +   resource \"null_resource\" \"bar\" {\n\
         +       id = (known after apply)\n\
        \    }\n\n\
        \  # null_resource.baz will be created\n\
         +   resource \"null_resource\" \"baz\" {\n\
         +       id = (known after apply)\n\
        \    }\n\n\
        \  # null_resource.foo will be created\n\
         !   resource \"null_resource\" \"foo\" {\n\
         -       id = (known after apply)\n\
        \    }\n\n\
         Plan: 3 to add, 0 to change, 0 to destroy."
      in
      assert (CCString.equal plan_diff (Terrat_plan_diff.transform plan_text)))

let test_heredoc_yaml =
  Oth.test ~name:"Test heredoc yaml dashes are not treated as removals" (fun _ ->
      (* A YAML list inside a Terraform heredoc value uses [- ] for list items.
         Those dashes must not be promoted to removal markers, otherwise the
         diff shows unchanged content as being removed. The heredoc opener and
         the surrounding resource lines are still real diff lines and must be
         transformed as usual. *)
      let plan_text =
        CCString.concat
          "\n"
          [
            "  ~ resource \"helm_release\" \"foo\" {";
            "      ~ values = <<-EOT";
            "            \"appListenPorts\":";
            "            - \"name\": \"http-internal\"";
            "              \"port\": 9880";
            "            - \"name\": \"http-public\"";
            "              \"port\": 9881";
            "            \"env\":";
            "            - \"name\": \"API_SERVER_ENABLED\"";
            "              \"value\": \"1\"";
            "        EOT";
            "    }";
          ]
      in
      let plan_diff =
        CCString.concat
          "\n"
          [
            "!   resource \"helm_release\" \"foo\" {";
            "!       values = <<-EOT";
            "            \"appListenPorts\":";
            "            - \"name\": \"http-internal\"";
            "              \"port\": 9880";
            "            - \"name\": \"http-public\"";
            "              \"port\": 9881";
            "            \"env\":";
            "            - \"name\": \"API_SERVER_ENABLED\"";
            "              \"value\": \"1\"";
            "        EOT";
            "    }";
          ]
      in
      assert (CCString.equal plan_diff (Terrat_plan_diff.transform plan_text)))

let test_heredoc_custom_delimiter_and_multiple =
  Oth.test ~name:"Test heredoc handles custom delimiters and multiple heredocs" (fun _ ->
      (* The heredoc delimiter is an arbitrary identifier, not necessarily EOT.
         A diff can also contain several heredocs in a row; the body of one must
         not swallow the opener of the next, so the closing delimiter has to be
         matched exactly to resume normal transformation. *)
      let plan_text =
        CCString.concat
          "\n"
          [
            "      ~ a = <<-EOF";
            "            - one";
            "        EOF";
            "      ~ b = <<HEREDOC_1";
            "            - two";
            "        HEREDOC_1";
            "    }";
          ]
      in
      let plan_diff =
        CCString.concat
          "\n"
          [
            "!       a = <<-EOF";
            "            - one";
            "        EOF";
            "!       b = <<HEREDOC_1";
            "            - two";
            "        HEREDOC_1";
            "    }";
          ]
      in
      assert (CCString.equal plan_diff (Terrat_plan_diff.transform plan_text)))

let test =
  Oth.parallel [ test_simple; test_heredoc_yaml; test_heredoc_custom_delimiter_and_multiple ]

let () =
  Random.self_init ();
  Oth.run test

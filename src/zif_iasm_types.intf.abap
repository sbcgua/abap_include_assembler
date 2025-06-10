interface zif_iasm_types public.

  types:
    tt_class_names type standard table of seoclasstx-clsname with default key,
    ts_class_names type sorted table of seoclasstx-clsname with unique key table_line.

  types:
    tt_prog_names type standard table of trdir-name with default key.

  types:
    begin of ty_rename,
      from type seoclasstx-clsname,
      to type seoclasstx-clsname,
    end of ty_rename.

  types ts_renames type sorted table of ty_rename with unique key from.

endinterface.

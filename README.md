# Aspirational README. WIP.

`trek` is a simple database migrator. It has two commands: `create` and `apply`.

## `trek create`

### Example
```bash
$ trek create NAME
```
### Description

`create` uses current date time to make a sql file in the proper format for `apply`.

`create NAME` will make a `NAME-VERSION.sql` file in the path pointed at by `NAME`.

`create` will return a exit code of `1` if the file creation fails for any reason.

## `trek apply`

### Example
```bash
$ trek apply DIRPATH
{ "created_at" : "2020-01-01T22:31:04"
, "migrations" :
    [ { "name"    : "foo"
      , "version" : "2019-01-01T22:31:04"
      , "hash"    : "xofdshagnosfdasngs"
      }
    , { "name"    : "bar"
      , "version" : "2019-02-01T22:31:04"
      , "hash"    : "barbar"
      }
    ]
}
```

### Description

`apply` applies the migrations in the `DIRPATH`.

If `DIRPATH` is not in the proper format `apply` will fail.
- if `DIRPATH` contains zero `*.sql` files `apply` returns a exit code of `1`.
- if any `*.sql` files cannot be parsed in the `NAME-VERSION.sql` format `apply` will return exit code `2`.

Otherwise `apply` will execute the non-applied queries in `DIRPATH`
together in a single `SERIALIZABLE` transaction.

If any of the sql files fail the transaction will be aborted and `trek` will return an exit code of `3`.

`apply` returns a JSON list of migrations entries if the exit code is `0` as seen in the example above. *The formatting of `trek` JSON is not shown*.

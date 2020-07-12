[![Travis CI Status](https://travis-ci.org/jfischoff/trek.svg?branch=master)](http://travis-ci.org/jfischoff/trek)
# Aspirational README. WIP.

`trek` is a simple database migrator. It has two commands: `create` and `apply`.

## `trek create`

### Example
```bash
$ trek create path/migration.sql
path/2020-07-09T06-21-12-migration.sql
```
### Description

#### Successful Behavior

`create NAME` will make a `VERSION-NAME` file in the path pointed at by `NAME` using the current date time as `%Y-%m-%dT%H-%M-%S`.

#### Errors

`create` will return a exit code of `4` if the directory to the migration does not exist. Otherwise it will return `1` if an unknown error occurs.

## `trek apply`

### Example
```bash
$ trek apply DIRPATH
{
    "migrations": [
        {
            "hash": "TBCJw6kwo28hSb39YPrLk9fI4IlToG17s+PwG8JQLUk=",
            "version": "2020-07-12T06:21:21Z"
        },
        {
            "hash": "A+Gb17iQkxBuDsOvNKfQfi30U5I510vfaa0E0UEnWjc=",
            "version": "2020-07-12T06:21:27Z"
        },
        {
            "hash": "ECrdIikOzM8ZZG1uCMXpYdQtTkGFO/+nLwmFi6PeVHo=",
            "version": "2020-07-12T06:21:32Z"
        }
    ],
    "created_at": "2020-07-12T18:29:04.7395Z",
    "id": "KgnJYCdJlarNU25q8SZDCPIfXxs="
}
```

### Description

#### Successful Behavior

`apply` executes the the non-applied migrations (`*.sql` files) in `DIRPATH` together in a single `READ COMMITTED` transaction.

`apply` returns a JSON list of migrations entries as seen in the example above.

#### Errors

- If any of the sql files fail the transaction will be aborted and `trek` will return an exit code of `1`.
- if any `*.sql` files cannot be parsed in the `NAME-VERSION.sql` format `apply` will return exit code `2`.

#### Environment Variables

You can configure `trek` by setting the standard postgres connection [environment parameters](https://www.postgresql.org/docs/current/libpq-envars.html) or via command line arguments. See `apply --help` for details.

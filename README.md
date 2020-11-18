[![Travis CI Status](https://travis-ci.org/jfischoff/trek.svg?branch=master)](http://travis-ci.org/jfischoff/trek)

`trek` is a simple database migrator. It has the commands: 
- `create` 
- `apply`
- `list`
- `set-migrated`

## `trek create`

### Example
```bash
$ trek create path/migration.sql
path/2020-07-09T06-21-12_migration.sql
```

If a migration needs to run outside of a transaction, then the
`--no-transaction` flag may be passed:

```bash
$ trek create --no-transaction path/migration.sql
path/2020-07-09T06-21-12_migration_NO-TRANSACTION.sql
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

If the file name contains `_NO-TRANSACTION` as the last portion of the name
(before `*.sql`), then it will be run without a transaction. With multiple
files, they will be split into contiguous, ordered groups, so that adjacent
files will be run in the same transaction, and `NO_TRANSACTION` files will each
be run separately.

`apply` returns a JSON list of migrations entries as seen in the example above.

#### Errors

- If any of the sql files fail the transaction will be aborted and `trek` will return an exit code of `1`.
- if any `*.sql` files cannot be parsed in the `NAME-VERSION.sql` format `apply` will return exit code `2`.

## `trek set-migrated`

### Example
```bash
trek set-migrated --start-version=2020-07-12T05:00:00 --end-version=2020-07-12T07:00:00 DIRPATH
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

#### Successful Behavior
`set-migrated` adds database entries for all of the migrations that have not been applied between the range specified by `--start-version` and `--end-version` inclusively.

If `--start-version` or `--end-version` are not specified then the first or last migration is used in the specified folder `DIRPATH`.

On success `set-migrated` returns the migration group of the migrations it added to the database.

#### Errors
- if any `*.sql` files cannot be parsed in the `NAME-VERSION.sql` format `apply` will return exit code `2`.

## `trek list`

### Example
```bash
trek list
[ {
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
]

```

#### Successful Behavior
`list` will list all the applied migration groups.

## Environment Variables

You can configure `trek` by setting the standard postgres connection [environment parameters](https://www.postgresql.org/docs/current/libpq-envars.html) or via command line arguments. See `apply --help` for details.

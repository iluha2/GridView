# GridView

GridView is a Lazarus grid component for displaying records, arrays, collections, and any other data in the form of tables. It is a native and fast virtual grid without internal data storage designed to display external data using event handlers.


## Features

- Event-driven architecture.
- Up to 2147483647 rows.
- Multi-level header with column click support.
- Windows/Linux support, with themes.
- Unicode support.
- DB version included with multiselecting.


## How to use

1. Place TGridView component on form.
2. Define columns using "Columns" property.
3. Set row count using "Rows.Count" property.
4. Use OnGetCellText event to define cell text using your data.
5. Use OnSetEditText to get text into your data.


## Authors

Roman M. Mochalov (<checker@mail.ru>)
Iluha Companets (<iluha2@e1.ru>)


## License

[MIT](https://github.com/iluha2/GridView/blob/master/LICENSE)

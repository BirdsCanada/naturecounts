# naturecounts design principles

## Locations
- API urls
  - are stored in `meta_info`, a tibble created in `data-raw/data_creation.R`
  - This is where you can change from main to sandbox versions
  - https://birdscanada.org/api vs. https://sandbox.birdscanada.org/api
  - To apply this you must **re-run** `data-raw/data_creation.R` and then
    **re-load** the functions/package

## Fields returned by the API

#### Desired behaviour
Required and extra fields returned by the API are expected and treated, respectively, as follows:

Function           | API Entry Point           | Required fields            | Extra fields
------------------ |-------------------------- | -------------------------- | ------------
`nc_count()`       | `data/list_collections`   |  `collection`, `nrecords`  | Ignored
`nc_count()`       | `data/list_permissions`   |  `collection`, `akn_level` | Ignored
`nc_count()`       | `metadata/collections`    |  `collection`, `akn_level` | Ignored
`nc_permissions()` | `data/list_permissions`   |  `collection`, `akn_level` | Ignored
`srv_auth()`       | `data/authenticate`       |  `token`                   | Ignored
`nc_data_dl()`     | `data/release_request_id` |  None (close request only) | Ignored
`nc_data_dl()`     | `data/get_data`           |  Any                       | Added
`meta_XXX()`       | `metadata/XXX`            |  Any                       | Added

#### What will break the package
- Missing "Required fields" 
- Missing/renamed "container"  
  Some API entry points return data in a container, for example, 
  
  - `data/get_data` -> `results` (holds main data)
  - `data/list_collections` -> `results` (holds counts); `request_id` (holds request id)
  - `data/list_requests` -> `requests` (holds details on individual requests)

  If these "containers" change names, the package will break.

#### Coding Principles
- In package, after accessing the API, explictly `select()` the fields/columns expected. This way extra fields won't break existing code 
  - Missing fields **will** break the code, but at least they will break the code early!
  - Do **not** do this for data downloads (i.e. `nc_single_dl()` under `nc_data_dl()`) or metadata downloads (i.e. `meta_XXX()`)
  - **Unless**, using a `meta_XXX()` download internally. Then always `select()` the fields required

## Testing
- Tests are run using the user "testuser"
  - Locally password can be stored in .Renviron as naturecounts_testuser = PASSWORD
  - For remote testing, password is supplied as encrypted values

# fp-2024
## Domain - book library
**Main entities:**
* Books
  * title
  * author
  * genre
  * year
  
* Collections
  * name
  * description
  * books
  * other collections

**Main operations:**
* Add/remove book (add_book, remove_book)
* Create/remove book collection (create_collection, remove_collection)
* Add/remove book to/from collection (add_to_collection, remove_from_collection)
* List books/collections (list_books, list_collection)
* Add second collection to the first collection (combine_collections)
<!-- (is the last option recursive?) -->

Examples:
* add_book "Logika ir dirbtinis intelektas" "S. Norgėla" "Matematinė logika" "2007"
* create_collection "Matematinės logikos knygos" "Knygos apie logiką"
* add_to_collection "Matematinės logikos knygos" "Matematinė logika"
* create_collection "Matematika" "Visos knygos susiejusios su matematika"
* combine_collections "Matematika" "Matematinės logikos knygos"

**repl's grammar (BNF):**

[BNF](BNF.txt)
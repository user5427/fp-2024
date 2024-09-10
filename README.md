# fp-2024
## Domain - book library
**Main entities:**
* Books
  * title
  * author
  * genre
  * year
* Authors
* Readers

**Main operations:**
* Add/remove book (add-book, remove-book)
* Create book collection (create-collection, )
* Add/remove book to/from collection (add-to-collection, remove-from-collection)
* List books/collections (list-books, list-collection)

Examples:
* add-book "Logika ir dirbtinis intelektas" "S. Norgėla" "Matematinė logika" "2007"
* create-collection "Matematinės logikos knygos" "Knygos apie logiką".
* add-to-collection "Matematinės logikos knygos" "Matematinė logika"
<!-- (is the last option recursive?) -->

Examples:


**repl's grammar (BNF):**

[BNF](BNF.txt)
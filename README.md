# Flickell
CLI written in Haskell to help with Flickr batch downloads.

## Usage
```bash
cabal run -- -k [your-Flickr-key] -i [set-id]
```

## To do
- [x] Perform HTTPS request (fetching a JSON)
- [x] JSON to data type
- [x] Request photo(s) from the JSON received
- [x] Download photo(s) & save into file(s)
- [x] Get set ID as command line argument
- [ ] Work around `unsafePerformIO` to improve code
- [ ] Add verbose mode
- [ ] Progress info/bar

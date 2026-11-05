# Deploying the web app on a department server (for IT)

The app is a **static website**: a folder of files. No R, no database and no server-side program are needed. The analysis runs in each user's browser (R compiled to WebAssembly), and uploaded data never leave the user's computer.

## Build the folder

On a computer with R 4.5 and the packages `shiny`, `shinylive`, `ggplot2`, `writexl`, `readxl`, `agricolae`, `lmtest`, `mctest`:

```
Rscript scripts/build_app.R
Rscript prototype/tests/test_server.R        # all lines must say "ok"
Rscript scripts/export_site.R site
```

Copy the whole `site` folder to the web server (about 80 MB; browsers download only part of it, and cache it).

## Server requirements

1. **HTTPS** (or `localhost`). The app uses a service worker and the clipboard, which browsers allow only on secure pages. A plain `http://` address on the network will show a blank page.
2. **MIME types**: `.wasm` as `application/wasm`, `.js` as `text/javascript`, `.json` as `application/json`. Most servers already do this; if the page stays blank, check these first.
3. Serve `index.html` at the site root or in a sub-folder (both work). Do not rename files.
4. Optional but recommended: gzip or brotli compression for `.js`, `.wasm`, `.data`, `.gz` files; long cache lifetime for `shinylive/`.

## Network requirement to check

On first load the browser may download some R packages from the public WebAssembly repository (`repo.r-wasm.org`) and fetch assets from the internet. If users' computers cannot reach the internet, the page may not start. Test on a user's computer in the department network before announcing it. If it fails, tell the maintainer: the packages can be bundled on the server instead.

## Check after installing

Open the address in Chrome or Edge. Wait for the app to load (the first time takes longer). Open the **About** tab and confirm the version line; run one built-in example in each analysis and download an Excel file.

## Counting visitors

The server's access log is enough (requests for `index.html`). The app itself collects nothing.

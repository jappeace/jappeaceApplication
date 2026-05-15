# Technical Feasibility Report: Panzer-Shop Migration
## MijnWebwinkel to Shopify — Data Migration

**Client**: Kevin (Panzer-ShopNL)
**Date**: 2026-05-15
**Status**: Feasibility assessment

## Current Situation

Kevin runs an online model train and military miniature shop across 3 domains:

| Language | Domain              | Role          |
|----------|---------------------|---------------|
| German   | panzer-shopnl.de    | Primary store |
| English  | panzer-shop.net     | English store |
| Dutch    | panzer-shop.nl      | Dutch store   |

The store runs on **MijnWebwinkel** (internationally branded as MyOnlineStore).
It contains approximately **2400 products** across **~50 categories** in **3 languages**.

Each product has: title, description, images (up to 15), price, meta title, meta description — all translated per language.

Kevin already has a Shopify account (built 2-3 years ago, currently offline) but never migrated because doing 2400 x 3 languages by hand is not feasible.

### URL Structure (current)

- Products: `/a-{numeric_id}/{url-slug}/` (e.g., `/a-126324428/rettungsdienste/tt-mercedes-benz-e-klasse-limousine/`)
- Categories: `/c-{numeric_id}/{url-slug}/` (e.g., `/c-5597278/1-160-n-spur/`)

## Source: MijnWebwinkel API

MijnWebwinkel has a public REST API at `connect.myonlinestore.com` ([documentation](https://github.com/MyOnlineStore/public-api-docs)).

### Available Endpoints

| Endpoint                                | Method | Purpose                   |
|-----------------------------------------|--------|---------------------------|
| `/v{version}/articles`                  | GET    | List all products         |
| `/v{version}/articles/{id}`             | GET    | Get single product        |
| `/v{version}/articles/count`            | GET    | Count total products      |
| `/v{version}/articles/uploadImage/{id}` | POST   | Image management          |
| `/v{version}/categories/{id}/articles`  | GET    | Products in a category    |
| `/v{version}/articlefields/{id}`        | POST   | Custom fields             |

### Available Product Fields

- **Content**: `name` (title), `description` (HTML), `sku`, `url`
- **SEO**: `meta_title`, `meta_description`
- **Pricing**: `price.default`, `price.action` (sale), `price.purchase` (cost)
- **Media**: `images` array with URLs in multiple sizes
- **Inventory**: `stock`, `delivery_days`, `can_backorder`
- **Organization**: `categories`, `lists`, `variants`
- **Custom**: `extra.visible` (specs like weight, dimensions), `extra.invisible`
- **Metadata**: `uuid`, `created_date`, `updated_date`

### Multilingual Support

The API accepts a `?language=` query parameter supporting: Dutch, English (GB/US), German, French, Spanish, Portuguese, Italian, Danish, Swedish, Norwegian, Turkish, Polish.

To export all 3 languages, we make 3 API calls per page of results, each with a different language parameter. The API returns translated content for that language.

### Access Requirements

- Requires a **Premium subscription** (Kevin needs to confirm he has this)
- API token generated in admin panel
- Supports both JSON and XML responses

## Target: Shopify

### Product Import

Shopify supports CSV import with 51 columns. Key columns:

- `Handle` — URL slug (used to group variants)
- `Title`, `Body (HTML)` — product content
- `Vendor`, `Product Category`, `Type`, `Tags`
- `Variant SKU`, `Variant Price`, `Variant Compare At Price`
- `Image Src`, `Image Position`, `Image Alt Text`
- `SEO Title`, `SEO Description`
- `Status` — active/draft/archived

The native CSV import only handles the **default language**. Translations require a separate step.

### Translation Import (for secondary languages)

Shopify stores translations separately from products. Three options:

1. **Shopify GraphQL Admin API** — `translationsRegister` mutation. Programmatic, full control. Requires `read_products`, `write_translations`, `write_locales` scopes. Each translation needs: `locale`, `key` (e.g. "title", "body_html"), `value`, and `translatableContentDigest` (from a prior query). **This is the approach we will use.**

2. **Altera app** (~$20/month) — bulk spreadsheet import of translations. Matrixify-compatible format. Good for manual corrections.

3. **Translate & Adapt** (free Shopify app) — CSV import of translations via Settings > Languages. Clunky for 2400 products, exports everything at once.

### Multilingual Infrastructure: Shopify Markets

Shopify Markets handles internationalization:

- Supports up to 20 languages per store
- Automatically creates subfolder URL structure (`/de/`, `/en/`, `/nl/`)
- Automatically generates `hreflang` tags (critical for SEO)
- Handles currency localization per region
- Available on all plans (Basic and up)

### 301 Redirects

Shopify has built-in URL redirect support:

- CSV bulk import with two columns: `Redirect from`, `Redirect to`
- `Redirect from`: relative path only (e.g., `/a-126324428/some-slug/`)
- `Redirect to`: relative path for internal, full URL for external
- Maximum **100,000 redirects** on standard plans (20M on Plus)
- Redirects only fire on 404 pages
- Available at Content > Menus > URL Redirects > Import

## Testing: Shopify Partners Development Store

Shopify Partners program is **free to join** at [partners.shopify.com/signup](https://partners.shopify.com/signup).

Dev stores provide:

- Unlimited free development stores
- Full CSV product import support
- Full GraphQL Admin API access
- Shopify Markets and translation support
- URL redirect import support
- Advanced plan features included

Limitations (none affect migration testing):

- No real payment processing (test orders only)
- Password-protected storefront
- Custom domains don't resolve
- No paid app installs

We will use a dev store to run and verify the full migration before touching Kevin's production Shopify store.

## Migration Strategy

### Phase 1: Extract from MijnWebwinkel

1. Authenticate with MijnWebwinkel API
2. Paginate through all products in German (default language)
3. For each product, also fetch English and Dutch translations
4. Download all product images to local storage
5. Extract category structure and mappings

### Phase 2: Transform

1. Map MijnWebwinkel product fields to Shopify CSV columns
2. Generate Shopify product CSV (German as default language)
3. Prepare translation payloads for English and Dutch (for GraphQL API)
4. Generate 301 redirect CSV mapping all old URLs to new Shopify URLs
5. Map old categories to Shopify collections

### Phase 3: Import to Shopify (test store first)

1. Import products via CSV
2. Register translations via GraphQL `translationsRegister`
3. Import 301 redirects via CSV
4. Create collections matching old category structure

### Phase 4: Verify

1. For every product: assert it exists in Shopify with correct title, description, price in all 3 languages
2. For every product: verify images are present and accessible
3. For every product: verify SEO title and SEO description in all 3 languages
4. For every redirect: verify old URL path redirects to correct new URL
5. Verify hreflang tags are present and correct
6. Generate verification report with pass/fail per product

### Phase 5: Go Live

1. Run migration against Kevin's production Shopify store
2. Verify again on production
3. Update DNS: point panzer-shopnl.de, panzer-shop.net, panzer-shop.nl to Shopify
4. Submit updated sitemaps to Google Search Console
5. Monitor Google Search Console for crawl errors for first 3 months

## Risk Assessment

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| MijnWebwinkel API doesn't expose all fields | Low | API docs confirm all needed fields are available |
| API rate limiting | Medium | Implement pagination with backoff; 2400 products is modest |
| Image download issues | Low | Retry logic; images served from CDN |
| Translation API complexity | Medium | Need to query `translatableContentDigest` before registering; well-documented |
| Category structure mismatch | Low | Shopify collections are flexible; manual review of mapping |
| SEO ranking drop during cutover | Medium | 301 redirects + sitemap submission; expected recovery 2-4 weeks for this site size |
| Kevin doesn't have Premium (API access) | Medium | Ask him to confirm; fallback is web scraping |

## Technical Implementation

The migration tool will be a Haskell program with:

- HTTP client for MijnWebwinkel API (aeson for JSON parsing)
- Shopify Admin API client (GraphQL via HTTP)
- CSV generation for Shopify product import and redirect import
- Image downloader with retry logic
- Verification module that crawls the Shopify store post-import
- Structured logging and error reporting

## Conclusion

The migration is technically feasible. All required data fields are accessible via the MijnWebwinkel API, and Shopify supports all the import mechanisms needed (CSV for products and redirects, GraphQL for translations). A free Shopify Partners dev store provides a complete testing environment.

### What Kevin Needs to Provide

1. MijnWebwinkel API token (confirm Premium subscription)
2. Shopify Admin API access token
3. DNS registrar credentials (for cutover)

### Quoted Price

**EUR 500 flat fee** — includes full data migration, SEO redirect setup, verification, and DNS cutover support.

{ lib, stdenv, fetchFromGitHub, fetchurl, fetchPnpmDeps, pnpmConfigHook
, pnpm_8, nodejs, makeWrapper
}:

let
  # The npm tarball contains pre-built dist/ (compiled from TypeScript).
  # The GitHub source's rollup build fails on this Node version, so we
  # copy dist/ from the published tarball instead.
  npmDist = fetchurl {
    url = "https://registry.npmjs.org/seo-analyzer/-/seo-analyzer-3.2.0.tgz";
    hash = "sha256-J15KGRIN5wVjNrajlzuMbwwSmsgv/HOmJ4C4qXSpPcg=";
  };
in
stdenv.mkDerivation (finalAttrs: {
  pname = "seo-analyzer";
  version = "3.2.0";

  src = fetchFromGitHub {
    owner = "maddevsio";
    repo = "seo-analyzer";
    rev = "8ec76a346224e88a0508033a6494e34e5707aa73";
    hash = "sha256-AlQRqwsmgST2N1TflRuicul9Rs5f48FSZ61PoIzGoEs=";
  };

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    pnpm = pnpm_8;
    fetcherVersion = 3;
    hash = "sha256-mEWG+UEVEFNasLUAF8dGjtkZqPNs0rzgLkifDskhN5I=";
  };

  nativeBuildInputs = [
    pnpmConfigHook
    pnpm_8
    nodejs
    makeWrapper
  ];

  postPatch = ''
    # Copy pre-built dist/ from npm tarball (TS build fails on this Node)
    tar xzf ${npmDist} package/dist --strip-components=1

    # Fix bin/analyzer.js: upstream addRule only accepts functions but the
    # CLI passes rule name strings. Resolve names to functions via getDefaultRules().
    cat > bin/analyzer.js <<'BINEOF'
const SeoAnalyzer = require('../dist/index.js');

const IGNORES = {
  ignoreFiles: 'ignoreFiles',
  ignoreFolders: 'ignoreFolders',
  ignoreUrls: 'ignoreUrls'
};

const INPUTS = {
  files: 'inputFiles',
  folders: 'inputFolders',
  urls: 'inputUrls'
};

function formatRuleParams(input) {
  if (!input) return null;
  try { return JSON.parse(input); }
  catch { console.error("Rule params must be valid JSON"); process.exit(1); }
}

function detectOptions(options, obj) {
  return Object.keys(obj)
    .filter(key => options[key])
    .map(key => ({ name: obj[key], value: options[key] }));
}

module.exports = options => {
  const analyzer = new SeoAnalyzer();
  const defaultRules = analyzer.getDefaultRules();

  for (const { name, value } of detectOptions(options, IGNORES))
    analyzer[name](value);

  for (const { name, value } of detectOptions(options, INPUTS))
    analyzer[name](value);

  // Resolve rule names from CLI (-r) to actual functions from defaultRules.
  // Upstream addRule only accepts functions, not strings.
  const cliRules = options.rules || [];
  const ruleEntries = cliRules.length > 0
    ? cliRules.map(r => {
        const ruleName = r.split('=')[0];
        const ruleOpts = formatRuleParams(r.split('=')[1]);
        const ruleFn = defaultRules[ruleName];
        if (!ruleFn) { console.error("Unknown rule: " + ruleName); process.exit(1); }
        return { fn: ruleFn, opts: ruleOpts || {} };
      })
    : Object.values(defaultRules).map(fn => ({ fn, opts: {} }));

  for (const { fn, opts } of ruleEntries)
    analyzer.addRule(fn, opts);

  analyzer.outputConsole().run();
};
BINEOF
  '';

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/node_modules/seo-analyzer
    cp -r bin dist package.json $out/lib/node_modules/seo-analyzer/
    cp -r node_modules $out/lib/node_modules/seo-analyzer/

    mkdir -p $out/bin
    makeWrapper ${nodejs}/bin/node $out/bin/seo-analyzer \
      --add-flags "$out/lib/node_modules/seo-analyzer/bin/index.js"

    runHook postInstall
  '';

  meta = {
    description = "SEO analyzer for static HTML files";
    homepage = "https://github.com/maddevsio/seo-analyzer";
    license = lib.licenses.mit;
  };
})

{ lib, buildNpmPackage, fetchurl }:

buildNpmPackage rec {
  pname = "seo-analyzer";
  version = "3.2.0";

  # Use the npm registry tarball which already contains compiled JS (dist/)
  src = fetchurl {
    url = "https://registry.npmjs.org/seo-analyzer/-/seo-analyzer-${version}.tgz";
    hash = "sha256-J15KGRIN5wVjNrajlzuMbwwSmsgv/HOmJ4C4qXSpPcg=";
  };

  sourceRoot = "package";

  # The npm tarball is missing bin/version.js and bin/analyzer.js
  # (publishing bug upstream — files field only lists dist/).
  # Restore them and add bin/ to the files list so npmInstallHook copies them.
  postPatch = ''
    cp ${./seo-analyzer-package-lock.json} package-lock.json

    cat > bin/version.js <<'BINEOF'
const pkg = require('../package.json');
module.exports = () => { console.log(pkg.version); };
BINEOF

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

    # Add bin/ to files list so npm copies it during install.
    # Use sed instead of jq since jq isn't available in the npm deps derivation.
    sed -i '/"dist"/a\    ,"bin"' package.json
  '';

  npmDepsHash = "sha256-kA0Ikr94a6GvsNUTLEKmsZx50Coud2JEjaAzjFlHjZU=";

  dontNpmBuild = true;

  meta = {
    description = "SEO analyzer for static HTML files";
    homepage = "https://github.com/maddevsio/seo-analyzer";
    license = lib.licenses.mit;
  };
}

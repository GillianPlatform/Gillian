/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
  mount: {
    'src/webviews/public': { url: '/', static: true },
    'src/webviews/src': { url: '/' },
    'node_modules/@vscode/codicons/dist': { url: '/public', static: true },
  },
  plugins: [
    '@snowpack/plugin-dotenv',
    '@snowpack/plugin-postcss',
    [
      '@snowpack/plugin-typescript',
      { args: '--project ./tsconfig-webview.json' },
    ],
  ],
  routes: [
    /* Enable an SPA Fallback in development: */
    // {"match": "routes", "src": ".*", "dest": "/index.html"},
  ],
  optimize: {
    bundle: true,
    minify: true,
    target: 'es2020',
    entrypoints: ['index.js'], // one per webview
  },
  packageOptions: {},
  devOptions: {
    output: 'stream',
  },
  buildOptions: {
    out: 'out/webviews',
  },
}

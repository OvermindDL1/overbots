import node_resolve from 'rollup-plugin-node-resolve';

export default {
  entry: './lib/es6/src/overbots.js',
  format: 'iife',
  dest: './release/overbots-bundled.js',
  moduleName: 'overbots',
  plugins: [
    node_resolve({
      module: true,
      browser: true
    })
  ]
};

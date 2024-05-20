import { nodeResolve } from "@rollup/plugin-node-resolve";

export default {
  base: "./",
  build: {
    outDir: "./out",
  },
  plugins: [nodeResolve()],
};

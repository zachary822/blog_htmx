import { resolve } from "path";

/**
 * @type {import('vite').UserConfig}
 */
const config = {
  server: {
    proxy: {
      "/posts": {
        target: "http://localhost:3000",
        changeOrigin: true,
        bypass: (req, res, options) => {
          if (req.headers["hx-request"] === "true") {
            return null;
          }
          return "/";
        },
      },
      "/images": {
        target: "https://blog2.thoughtbank.app",
        changeOrigin: true,
      },
    },
  },
  build: {
    rollupOptions: {
      input: {
        main: resolve(__dirname, "index.html"),
        404: resolve(__dirname, "404.html"),
      },
    },
  },
};

export default config;

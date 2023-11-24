/**
 * @type {import('vite').UserConfig}
 */
const config = {
  server: {
    proxy: {
      "/posts": {
        target: "http://localhost:3000",
        changeOrigin: true,
      },
    },
  },
};

export default config;

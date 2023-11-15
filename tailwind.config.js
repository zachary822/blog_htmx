/** @type {import('tailwindcss').Config} */
export default {
  content: ["./index.html", "./main.js", "./app/**/*.hs"],
  theme: {
    extend: {},
  },
  plugins: [require("@tailwindcss/typography"), require("daisyui")],
};

/** @type {import('tailwindcss').Config} */
export default {
  content: ["./index.html", "./main.js", "./app/**/*.hs"],
  theme: {
    extend: {
      transitionProperty: {
        "background-size": "background-size",
      },
      backgroundSize: {
        underline: "0% 3px",
        "underline-hover": "100% 3px",
      },
    },
  },
  plugins: [require("@tailwindcss/typography"), require("daisyui")],
};

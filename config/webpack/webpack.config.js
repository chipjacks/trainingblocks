const path = require("path");
const webpack = require("webpack");
const isProduction = process.env.NODE_ENV === "production";
const elmSource = path.resolve(process.cwd());
const elmMake = `${elmSource}/node_modules/.bin/elm`;

const elmDefaultOptions = {
  cwd: elmSource,
  pathToElm: elmMake,
  optimize: true,
};
const developmentOptions = Object.assign({}, elmDefaultOptions, {
  optimize: false,
  verbose: false,
  debug: true,
});

const elmWebpackLoader = {
  loader: "elm-webpack-loader",
  options: isProduction ? elmDefaultOptions : developmentOptions,
};

module.exports = {
  mode: "production",
  devtool: "source-map",
  entry: {
    account: "./app/javascript/packs/account.js",
    calendar: "./app/javascript/packs/calendar.js",
    settings: "./app/javascript/packs/settings.js",
    trends: "./app/javascript/packs/trends.js",
  },
  output: {
    filename: "[name].js",
    sourceMapFilename: "[file].map",
    path: path.resolve(__dirname, "..", "..", "app/assets/builds"),
  },
  plugins: [
    new webpack.optimize.LimitChunkCountPlugin({
      maxChunks: 1,
    }),
  ],
  module: {
    rules: [
      {
        test: /\.elm(\.erb)?$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: isProduction
          ? [elmWebpackLoader]
          : [{ loader: "elm-hot-webpack-loader" }, elmWebpackLoader],
      },
    ],
  },
};

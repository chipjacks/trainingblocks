const { resolve } = require('path')

const isProduction = process.env.NODE_ENV === 'production'
const elmSource = resolve(process.cwd())
const elmPath = `${elmSource}/node_modules/.bin/elm`

const elmDefaultOptions = { cwd: elmSource, pathToElm: elmPath }
const developmentOptions = Object.assign({}, elmDefaultOptions, {
  verbose: false,
  warn: false,
  debug: true
})

const elmWebpackLoader = {
  loader: 'elm-webpack-loader',
  options: isProduction ? elmDefaultOptions : developmentOptions
}

module.exports = {
  test: /\.elm(\.erb)?$/,
  exclude: [/elm-stuff/, /node_modules/],
  use: elmWebpackLoader
}

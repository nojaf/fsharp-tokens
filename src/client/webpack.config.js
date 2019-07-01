const path = require("path");
const webpack = require("webpack");
const MinifyPlugin = require("terser-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const MonacoWebpackPlugin = require("monaco-editor-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");

function resolve(filePath) {
  return path.join(__dirname, filePath);
}

const CONFIG = {
  fsharpEntry: {
    app: [resolve("./App.fsx")]
  },
  devServerProxy: {
    "/api/*": {
      target: "http://localhost:" + (process.env.SUAVE_FABLE_PORT || "8085"),
      changeOrigin: true
    }
  },
  historyApiFallback: {
    index: resolve("./index.html")
  },
  contentBase: resolve("./public"),
  // Use babel-preset-env to generate JS compatible with most-used browsers.
  // More info at https://github.com/babel/babel/blob/master/packages/babel-preset-env/README.md
  babel: {
    presets: [
      [
        "@babel/preset-env",
        {
          targets: {
            browsers: ["last 2 versions"]
          },
          modules: false
        }
      ],
      "@babel/preset-react"
    ],
    plugins: ["@babel/plugin-proposal-class-properties"]
  }
};

const commonPlugins = [
  new HtmlWebpackPlugin({
    filename: resolve("./output/index.html"),
    template: resolve("./public/index.html")
  }),
  new MonacoWebpackPlugin({
    languages: ["fsharp", "javascript", "typescript"],
    features: [
      "accessibilityHelp",
      "bracketMatching",
      "caretOperations",
      "clipboard",
      "codelens",
      "colorDetector",
      "comment",
      "contextmenu",
      // 'coreCommands',
      "cursorUndo",
      // 'dnd',
      "find",
      // 'folding',
      // 'format',
      "gotoDeclarationCommands",
      "gotoDeclarationMouse",
      "gotoError",
      "gotoLine",
      "hover",
      "inPlaceReplace",
      "inspectTokens",
      // 'iPadShowKeyboard',
      "linesOperations",
      "links",
      "multicursor",
      "parameterHints",
      // 'quickCommand',
      // 'quickFixCommands',
      // 'quickOutline',
      // 'referenceSearch',
      // 'rename',
      "smartSelect",
      // 'snippets',
      "suggest",
      "toggleHighContrast",
      "toggleTabFocusMode",
      "transpose",
      "wordHighlighter",
      "wordOperations"
    ]
  })
];

const isProduction = process.argv.indexOf("-p") >= 0;
console.log(
  "Bundling for " + (isProduction ? "production" : "development") + "..."
);

module.exports = {
  entry: CONFIG.fsharpEntry,
  output: {
    path: resolve("./output"),
    filename: isProduction ? "[name].[hash].js" : "[name].js"
  },
  mode: isProduction ? "production" : "development",
  devtool: isProduction ? undefined : "source-map",
  optimization: {
    // Split the code coming from npm packages into a different file.
    // 3rd party dependencies change less often, let the browser cache them.
    splitChunks: {
      cacheGroups: {
        commons: {
          test: /node_modules/,
          name: "vendors",
          chunks: "all"
        }
      }
    },
    minimizer: isProduction ? [new MinifyPlugin()] : []
  },
  // DEVELOPMENT
  //      - HotModuleReplacementPlugin: Enables hot reloading when code changes without refreshing
  plugins: isProduction
    ? commonPlugins.concat([
        new MiniCssExtractPlugin({ filename: "style.css" }),
        new CopyWebpackPlugin([{ from: resolve("./public") }]),
        // ensure that we get a production build of any dependencies
        // this is primarily for React, where this removes 179KB from the bundle
        new webpack.DefinePlugin({
          "process.env.NODE_ENV": '"production"'
        })
      ])
    : commonPlugins.concat([new webpack.HotModuleReplacementPlugin()]),
  // Configuration for webpack-dev-server
  devServer: {
    proxy: CONFIG.devServerProxy,
    hot: true,
    inline: true,
    historyApiFallback: CONFIG.historyApiFallback,
    contentBase: CONFIG.contentBase,
    host: "0.0.0.0",
    watchOptions: {
      poll: true
    }
  },
  // - fable-loader: transforms F# into JS
  // - babel-loader: transforms JS to old syntax (compatible with old browsers)
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: "fable-loader"
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader",
          options: CONFIG.babel
        }
      },
      {
        test: /\.(sass|scss|css)$/,
        use: [
          isProduction ? MiniCssExtractPlugin.loader : "style-loader",
          "css-loader",
          "sass-loader"
        ]
      },
      {
        test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*$|$)/,
        use: ["file-loader"]
      }
    ]
  }
};

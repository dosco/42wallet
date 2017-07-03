# 42wallet
Easy and secure Ethereum HD wallet. Focused on Mobile and great UX

[https://42wallet.com](https://42wallet.com)

HD standard for "Hierarchical Deterministic" which estentially means that all you need to keep secret is a single phrase. 
Your accounts and private keys are all generated from this single phrase. You can have as many account as you need.

Using crypto-currency can be complicated for people. Our aim was to provide a more friendly and accessible user experience.
Additionally we wanted to ensure a high quality experience on mobile devices.

# Security
This is a client side wallet, that means it runs entirely in your browser. Important secrets like your private keys never
leave the browser. Even when making a transaction, it is signed with your private key within the browser and then submitted 
to an Ethereum node. Ensuring that your private keys never leave the browser is very important.

Addtionally the repo comes with Google Firebase Hosting configuration files. The hosted version of 42Wallet runs on Google
Firebase hosting. This is a very secure, high speed CDN backed static hosting service. This ensures that you are running 
"serverless" and with a high level of confidence.

We recommend you **DO NOT** try this host the wallet yourself on your own servers as security cannot be guranteed and 
sophisticated adversaires could modify the hosted app harming users. 

# Development
The wallet app is built in [Elm Lang](http://elm-lang.org/) a language that gurantees a very high quality web app with 
no runtime exceptions. It is described as "A delightful language for reliable webapps." and __I highly recommend you give
it a try__. 

[Install Elm Lang - Easy Installer](https://guide.elm-lang.org/install.html)

For all our development work we use [Visual Studio Code](https://code.visualstudio.com/) the open source IDE 
from Micosoft has an [Elm plugin](https://github.com/Krzysztof-Cieslak/vscode-elm) that makes it a solid IDE to use.

[Install Visual Studio Code](https://code.visualstudio.com/Download)

We use [Elm Reload](https://github.com/kingsleyh/elm-reload) to watch and rebuild the app. Elm has a very fast compiler so
this is almost instantaneous.

```
  cd web-elm
  elm-reload -w
```
  
For the other static HTML files within the wallets website I use [Hugo](https://gohugo.io/) a static website generator. Hugo
runs in a watch mode building the site and serving locally.

```
  cd web-site
  hugo --watch serve
```

You're local development environment is now setup and ready for you to being codeing. You're local build of the app is 
available at [http://localhost:1313](http://localhost:1313)


# Questions
Feel free to reach out to me on twitter at [@dosco](https://twitter.com/dosco)



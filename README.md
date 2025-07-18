<!-- Improved compatibility of back to top link: See: https://github.com/othneildrew/Best-README-Template/pull/73 -->
<a id="readme-top"></a>
<!--
*** Thanks for checking out the Best-README-Template. If you have a suggestion
*** that would make this better, please fork the repo and create a pull request
*** or simply open an issue with the tag "enhancement".
*** Don't forget to give the project a star!
*** Thanks again! Now go create something AMAZING! :D
-->



<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![project_license][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<div align="center">
<img src="https://raw.githubusercontent.com/hermitdesu/WordleBot/main/images/logo.png" alt="Logo" width="80" height="80">

<h3 align="center">WordleBot</h3>

  <p align="center">
    WordleBot is bot in telegram to play Wordle-like game.
    <a href="https://t.me/wordle_tele_bot">View Product</a>
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

[![wordlebot Screenshot][product-screenshot]](https://t.me/wordle_tele_bot)

`wordlebot` is a Telegram bot that lets users play a Wordle-style game directly inside Telegram chat. Built using [`telegram-bot-simple`](https://hackage.haskell.org/package/telegram-bot-simple), it features persistent user state, simple gameplay logic, and a clean, functional structure.

<p align="right">(<a href="#top">back to top</a>)</p>

---

## Getting Started

These instructions will help you set up and run the bot locally using [Stack](https://docs.haskellstack.org/).

### Prerequisites

Make sure you have the following installed:

- [Stack](https://docs.haskellstack.org/)
- [SQLite3](https://www.sqlite.org/)
- [dotenv CLI (optional)](https://github.com/motdotla/dotenv)

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/githubuser/wordlebot.git
   cd wordlebot
   ```

2. Copy the .env.example file and set your Telegram bot token:
```
cp .env.example .env
```

3. Then edit .env:
```
TELEGRAM_BOT_TOKEN=your_token_here
```
4. Build the project with Stack:
```
stack build
```
5. Run the bot:
```
    stack run
```
<p align="right">(<a href="#top">back to top</a>)</p> ```



<!-- USAGE EXAMPLES -->
## Usage

Just use the bot.

_For more examples, please refer to the [Documentation](https://example.com)_

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- ROADMAP -->
## Roadmap

- [ ] Feature 1
- [ ] Feature 2
- [ ] Feature 3
    - [ ] Nested Feature

See the [open issues](https://github.com/hermitdesu/WordleBot/issues) for a full list of proposed features (and known issues).

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#readme-top">back to top</a>)</p>

### Top contributors:

<a href="https://github.com/hermitdesu/WordleBot/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=hermitdesu/WordleBot" alt="contrib.rocks image" />
</a>



<!-- LICENSE -->
## License

Distributed under the project_license. See `LICENSE.txt` for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Your Name - [@twitter_handle](https://twitter.com/twitter_handle) - email@email_client.com

Project Link: [https://github.com/hermitdesu/WordleBot](https://github.com/hermitdesu/WordleBot)

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

* []()
* []()
* []()

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/hermitdesu/WordleBot.svg?style=for-the-badge
[contributors-url]: https://github.com/hermitdesu/WordleBot/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/hermitdesu/WordleBot.svg?style=for-the-badge
[forks-url]: https://github.com/hermitdesu/WordleBot/network/members
[stars-shield]: https://img.shields.io/github/stars/hermitdesu/WordleBot.svg?style=for-the-badge
[stars-url]: https://github.com/hermitdesu/WordleBot/stargazers
[issues-shield]: https://img.shields.io/github/issues/hermitdesu/WordleBot.svg?style=for-the-badge
[issues-url]: https://github.com/hermitdesu/WordleBot/issues
[license-shield]: https://img.shields.io/github/license/hermitdesu/WordleBot.svg?style=for-the-badge
[license-url]: https://github.com/hermitdesu/WordleBot/blob/master/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/linkedin_username
[product-screenshot]: images/screenshot.png
[Next.js]: https://img.shields.io/badge/next.js-000000?style=for-the-badge&logo=nextdotjs&logoColor=white
[Next-url]: https://nextjs.org/
[React.js]: https://img.shields.io/badge/React-20232A?style=for-the-badge&logo=react&logoColor=61DAFB
[React-url]: https://reactjs.org/
[Vue.js]: https://img.shields.io/badge/Vue.js-35495E?style=for-the-badge&logo=vuedotjs&logoColor=4FC08D
[Vue-url]: https://vuejs.org/
[Angular.io]: https://img.shields.io/badge/Angular-DD0031?style=for-the-badge&logo=angular&logoColor=white
[Angular-url]: https://angular.io/
[Svelte.dev]: https://img.shields.io/badge/Svelte-4A4A55?style=for-the-badge&logo=svelte&logoColor=FF3E00
[Svelte-url]: https://svelte.dev/
[Laravel.com]: https://img.shields.io/badge/Laravel-FF2D20?style=for-the-badge&logo=laravel&logoColor=white
[Laravel-url]: https://laravel.com
[Bootstrap.com]: https://img.shields.io/badge/Bootstrap-563D7C?style=for-the-badge&logo=bootstrap&logoColor=white
[Bootstrap-url]: https://getbootstrap.com
[JQuery.com]: https://img.shields.io/badge/jQuery-0769AD?style=for-the-badge&logo=jquery&logoColor=white
[JQuery-url]: https://jquery.com 

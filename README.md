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



<!-- PROJECT LOGO -->
<div align="center">
  <h3 align="center">WordleBot</h3>

  <p align="center">
    A simple Telegram bot to play a Wordle-like game written in Haskell.
    <br />
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

`wordlebot` is a Telegram bot that lets users play a Wordle-style game directly inside Telegram chat. Built using [`telegram-bot-simple`](https://hackage.haskell.org/package/telegram-bot-simple).

<p align="right">(<a href="#top">back to top</a>)</p>

## Roadmap

- [x] Simple Commands (/start, /help)
- [x] Game Logic
- [x] Change of language (for interface and wordlists)
- [x] Selection of difficulty with different amount of tries and wordlists
- [ ] Game Statistics

See the [open issues](https://github.com/hermitdesu/WordleBot/issues) for a full list of proposed features (and known issues).

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- USAGE EXAMPLES -->
## Usage

Use the [Telegram Bot]("https://t.me/wordle_tele_bot"), type /help to see availible comands.

<p align="right">(<a href="#readme-top">back to top</a>)</p>


## Getting Started

1. Clone the repository:
    
    ```bash
    git clone https://github.com/githubuser/wordlebot.git
    cd wordlebot
    ```

2. Copy the `.env.example` file and set your Telegram bot token:

    ```bash
    cp .env.example .env
    ```

3. Then edit `.env`:

    ```bash
    TELEGRAM_BOT_TOKEN=your_token_here
    ```

4. Build the project with Stack:

    ```bash
    stack build
    ```

5. Run the bot:

    ```bash
    stack run
    ```


<p align="right">(<a href="#top">back to top</a>)</p>





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

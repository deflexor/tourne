# Tourne

A terminal internet radio player with a Brick-based TUI, native audio via SDL2, and MP3 decoding via libmpg123.

## Features

- Browse and search radio stations from [radio-browser.info](https://www.radio-browser.info)
- Tag-based station discovery (jazz, classical, rock, pop, electronic, news, …)
- Native audio playback via SDL2 with queue-based PCM dispatch
- MP3 decoding via libmpg123
- Volume control
- Station ping/health monitoring
- Automatic failover between stations

## Prerequisites

### System Packages

| Package            | Purpose               | Debian/Ubuntu                      | Fedora                           |
|--------------------|-----------------------|------------------------------------|----------------------------------|
| GHC 9.6+           | Haskell compiler      | `ghc`                              | `ghc`                            |
| cabal-install      | Haskell build tool    | `cabal-install`                    | `cabal-install`                  |
| libsdl2-dev        | SDL2 headers/libraries| `libsdl2-dev`                      | `SDL2-devel`                     |
| libmpg123-dev      | MP3 decoding library  | `libmpg123-dev`                    | `mpg123-devel`                   |
| libpulse-dev       | PulseAudio (optional) | `libpulse-dev`                     | `pulseaudio-libs-devel`          |
| pkg-config         | Build-time discovery  | `pkg-config`                       | `pkgconfig`                      |

**Debian/Ubuntu one-liner:**

```bash
sudo apt-get install -y ghc cabal-install libsdl2-dev libmpg123-dev libpulse-dev pkg-config
```

### Haskell Setup

Ensure you have a recent GHC (9.6.x) and cabal-install. If not, use [ghcup](https://www.haskell.org/ghcup/):

```bash
ghcup install ghc 9.6.7
ghcup set ghc 9.6.7
ghcup install cabal 3.14
```

## Build & Run

```bash
# Clone the repo
git clone <repo-url> && cd tourne

# Build (first run downloads Haskell dependencies)
cabal build

# Run
cabal run tourne
```

## Controls

| Key         | Action                                            |
|-------------|---------------------------------------------------|
| `Tab`       | Switch focus (tags ↔ stations)                    |
| `↑` / `↓`  | Navigate list                                     |
| `Enter`     | Play selected station                             |
| `Space`     | Pause/resume                                      |
| `+` / `-`   | Volume up/down                                    |
| `/`         | Search stations                                   |
| `o`         | Cycle stations sort: name → bitrate → ping → name |
| `q` / `Esc` | Quit                                              |

## Configuration

The app uses sensible defaults defined in `Tourne.Config`. Key config values:

- **Volume**: 0.8 (80%)
- **API**: `https://de1.api.radio-browser.info`
- **Buffer**: 4096 PCM frames
- **Failover**: enabled

## Architecture

```
                   ┌─────────────────┐
                   │   radio-browser  │
                   │    .info API     │
                   └────────┬────────┘
                            │ HTTP
                   ┌────────▼────────┐
                   │  Tourne.Main    │
                   │  (Brick TUI)    │
                   └────────┬────────┘
                            │ commands
                   ┌────────▼────────┐
                   │  Audio.Player   │
                   │  (queueAudio)   │
                   └────────┬────────┘
                            │
              ┌─────────────┼─────────────┐
              │             │             │
       ┌──────▼─────┐ ┌────▼────┐ ┌──────▼─────┐
       │  Stream    │ │ Decoder │ │   SDL2     │
       │ (HTTP)     │ │ (mpg123)│ │ (PulseAudio)│
       └────────────┘ └─────────┘ └────────────┘
```

- **Stream**: Fetches MP3 data over HTTP from the selected station URL
- **Decoder**: Feeds MP3 frames through libmpg123, producing raw PCM `ByteString` chunks
- **Player**: Applies volume adjustment and queues PCM data directly to the SDL2 audio device via `SDL_QueueAudio` (bypassing the callback API)
- **TUI**: Brick-based terminal UI with tag/station lists, status bar, and keyboard input

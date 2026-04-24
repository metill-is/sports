# Modelled Leagues Reference

All leagues modelled in `Sports/`. Cross-reference against Lengjan availability when adding new scraping targets.

## Football

| Directory | Country | Leagues | In lengjan-odds? | On Lengjan? |
|-----------|---------|---------|-------------------|-------------|
| football_england | ENG | Premier League, Championship, League 1, League 2, FA Cup, EFL Cup | Yes (4 divisions) | Yes |
| football_italy | IT | Serie A, Serie B, Serie C (3 groups) | Yes (A, B) | Yes |
| football_spain | ES | LaLiga, LaLiga2, Primera RFEF, Copa del Rey | Yes (LaLiga, LaLiga2) | Yes |
| football_iceland | IS | Besta Deildin (+ upper/lower), Lengjudeild, Div 1-5 | No | No (off-season) |
| football_norway | NO | Eliteserien, OBOS-ligaen, Div 2A/2B, Div 3A/3B/3C | No | Cup only (off-season) |
| football_sweden | SE | Allsvenskan, Superettan, Div 1 Norra/Södra, Div 2 | No | No (off-season) |
| football_finland | FI | Veikkausliiga, Ykkösliiga, Ykkönen, Kakkonen (3 groups) | No | No (off-season) |
| football_brazil | BR | Série A/B/C/D, Copa do Brasil, Supercopa, Copa do Nordeste | No | Regional only |

## Basketball

| Directory | Country | Leagues | In lengjan-odds? | On Lengjan? |
|-----------|---------|---------|-------------------|-------------|
| basketball_iceland | IS | Bónusdeild (M+F), 1. Deild (M+F) | No | No (off-season) |
| basketball_international | Various | EuroBasket, World Cup, Olympics, Friendlies | No | — |

## Handball

| Directory | Country | Leagues | In lengjan-odds? | On Lengjan? |
|-----------|---------|---------|-------------------|-------------|
| handball_iceland | IS | Olís deild (M+F), Grill 66 deild (M+F), Cup | No | No (off-season) |
| handball_international | Various | EHF Euro (M+F), World Champ, Olympics, Friendlies | No | — |
| handball_other | DE | Bundesliga, 2. Bundesliga (M+F) | No | Yes (494, 493) |
| handball_other | DK | Herre-Handball-Ligaen, 1. Division (M+F) | No | Yes (490) |
| handball_other | FR | Starligue, Proligue (M+F) | No | Yes |
| handball_other | SE | Handbollsligan, Allsvenskan (M+F) | No | Yes |
| handball_other | NO | Rema 1000 Ligaen, 1. Division (M+F) | No | Yes |
| handball_other | ES | Liga ASOBAL, Division de Honor Plata (M+F) | No | Yes |
| handball_other | PL | Superliga, Central League (M+F) | No | Yes |
| handball_other | HU | NB I (M+F) | No | — |
| handball_other | AT | HLA (M) | No | — |
| handball_other | CZ | Extraliga (M) | No | — |
| handball_other | FI | Aktialiiga (M+F) | No | — |
| handball_other | PT | Andebol 1 (M+F) | No | — |

## Lengjan Sport IDs

- 1 = Football
- 2 = Basketball
- 6 = Handball

## Notes

- Nordic football/basketball leagues (IS, NO, SE, FI) are off-season Oct–Apr
- Iceland handball (IS) also off-season in summer
- Lengjan country availability changes seasonally — check before adding
- `handball_other/` has Lengjan configs in `R/utils/lengjan_info.R` with competition IDs for DE, DK, FR, SE, NO, ES, PL

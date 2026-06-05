# Bankrolls & Cross-Bookmaker Strategy

> Status: 2026-06-05. Personal financial-planning reference, not advice.
> **This is not legal or tax advice — confirm with an Icelandic lawyer / accountant (endurskoðandi) and RSK before acting on any money movement or tax position.**

## The three bankrolls

| Bookmaker | Balance | Legality (Iceland) | Odds quality | Automated? | Tax on winnings |
|---|---|---|---|---|---|
| **Lengjan** (Íslenskar getraunir) | **14,909 ISK** | **Strictly legal** — state-monopoly licensed | Worse | **Yes** — the pipeline only ever stakes here | **Tax-free** (RSK §8.4.2 names Icelandic getraunir) |
| **EpicBet** (Comeback N.V.) | **595 EUR** | Foreign, Curaçao/Estonia/Anjouan licence; **non-EEA**. Not licensed in Iceland; using it is **not penalised for the punter** | Better | No | **Likely taxable** foreign windfall; EEA exemption unlikely (non-EEA licence) |
| **CoolBet** (Polar Limited) | **541 EUR** | Foreign, Malta/Estonia/Sweden; **EEA**. Not licensed in Iceland; punter not penalised | Better | No | **Possibly tax-free** under EEA-lottery exemption *with documentation* — uncertain for a sportsbook; get a ruling |

**Foreign total: ~1,136 EUR.** Combined realised profit on the foreign books is large (the ledger's `handball iceland +99,880 ISK` and similar were placed on EpicBet/CoolBet, *not* Lengjan), but those gains are not attributable to a validated live edge — several came from leagues now on seasonal pause.

## Why Lengjan-only for automation

The user concentrated the automated pipeline on Lengjan despite its **worse odds** because **only Lengjan is strictly legal in Iceland** and its winnings are **tax-free**. Lengjan is also **not Cloudflare/anti-bot protected**, which is why the existing Chromote scraper/placer works against it. The foreign books have better odds and big historical profits but carry legality ambiguity, tax ambiguity, and (for any scraper) Cloudflare anti-bot friction.

## The bankroll correction (2026-06-05)

`current_pool` was **~7× over-stated**. `load_bankroll()` summed the whole ledger's settled PnL **across all three bookmakers** (there is no `bookmaker` column and no deposit/withdrawal tracking), giving ~107k ISK against a real Lengjan balance of **14,909 ISK** (29,000 ISK ever deposited to Lengjan, **down ~49%**). The live autoplace agent was therefore **over-staking ~7×**.

**Fix (`config/bankroll.yml`, PR #33):**
- `initial_pool: 29000` — total Lengjan deposits (drawdown reference): 5k (2025-09-25) + 5k (2026-03-04) + 9k (2026-03-05) + 10k (2026-05-13).
- `current_pool: 14909` — **set explicitly by hand**; real Lengjan balance incl. outstanding bets. No longer auto-derived from the cross-bookmaker ledger.
- The full ledger is **kept untouched** for historical profit analysis (backtest, by-cell PnL).
- `country == iceland` is **NOT** a Lengjan proxy — handball iceland (+99,880) was placed on the foreign books.

## Edge reality (the constraint on stake sizing)

The forensic review found **no demonstrable betting edge**: realised football 2026 ≈ **-8.1% on n=56**, indistinguishable from zero; all-time real ledger ≈ -3.7%. The defensive **kelly_frac cut to ~25% of Browne defaults** (football male 0.05, female 0.025 vs Browne 0.20 / 0.10–0.15) was an **artifact of the inflated pool**, not an edge judgement. Now that the pool is correctly small, a **partial** restore (toward ~half Browne, e.g. male ~0.10 / female ~0.05) is defensible *to undo the artifact* — but **levering up further on a zero/negative edge increases ruin risk**. Treat any kelly_frac increase as an explicit bounded experiment, re-checked after each batch of settled bets.

> Note: `daily_budget_min_isk = 1000` now **dominates** the 14,909 pool (1000 / 14,909 = 6.7%/day vs `daily_budget_frac = 0.05` → 745 ISK). `min_bet = 200` per bet; `kelly_ceiling = 0.25`.

## Legislation watch

No enacted bill and no live Alþingi bill with a timeline as of 2026. The reform direction (Viðskiptaráð / Chamber of Commerce) is **liberalising** — a licensing regime open to domestic *and* foreign operators (~22% operator duty), targeting **operators not punters**. No proposal penalises individuals holding/withdrawing foreign-book funds. The realistic downside to the user is **indirect**: future payment/transaction-blocking or tighter AML scrutiny on unlicensed-operator inflows — which argues for extracting the foreign balances **sooner rather than later**.

## Decision (2026-06-05)

1. **Extract** the ~1,136 EUR from EpicBet + CoolBet to the Icelandic IBAN promptly (KYC first; single withdrawal each to dodge EpicBet's 4% repeat-withdrawal fee; 1–2 weeks end-to-end).
2. **Keep Lengjan automated** as the strictly-legal, tax-free, no-anti-bot leg, now correctly sized to 14,909 ISK.
3. **Do NOT build CoolBet/EpicBet scrapers** — no proven edge to exploit, Cloudflare anti-bot friction, account-flagging risk, and legislation risk all point the wrong way.
4. **Leave foreign accounts dormant** (legal to hold) after extraction; reassess only if reform actually licenses them in Iceland.
5. **Tax:** report all foreign amounts on the framtal (RSK §2.8); Lengjan tax-free; CoolBet possibly exempt with documentation; EpicBet treat as declarable windfall — confirm with a professional.

### Sources
- RSK §8.4.2 Skattfrjálsir vinningar — https://leidbeiningar.rsk.is/frodi/?cat=1562&id=21646&k=8
- RSK §2.8 Tekjur erlendis — https://leidbeiningar.rsk.is/frodi/?cat=1342&id=19125&k=2
- Iceland betting legality overview — https://www.cheekypunter.com/country/iceland/ ; https://legalpilot.com/country/iceland/
- Gambling reform — https://www.igamingtoday.com/iceland-poised-for-gambling-reform-government-considers-opening-the-market/ ; https://www.igamingtoday.com/reforming-icelands-gambling-legislation/
- EpicBet review — https://www.askgamblers.com/sports-betting/sportsbook-reviews/epicbet-sportsbook
- CoolBet review — https://casino.guru/CoolBet-Casino-review

---

# Appendix A — Extraction plan (detail)

## Concrete extraction plan — getting ~1,136 EUR out

**NOT legal or tax advice. Withdrawal limits, fees, and KYC steps change; tax treatment of foreign winnings is genuinely unclear (see legality findings). Confirm specifics with each operator's live support and with an Icelandic accountant (endurskoðandi) / RSK before relying on tax-free treatment.**

### Mechanics (both books)

| | EpicBet (595 EUR) | CoolBet (541 EUR) |
|---|---|---|
| Licence | Curaçao GCB + Estonia + Anjouan (**non-EEA primary**) | Malta + Estonia + Sweden (**EEA**) |
| Bank/IBAN withdrawal | Yes ("Bank Transfer") | Yes ("Bank transfer"; also Trustly/Wise IBAN-based) |
| Bank-transfer timeline | **1–5 business days** (after a 0–72h pending hold) | **1–5 business days** (e-wallet faster: hours) |
| Daily max | 50,000 EUR (irrelevant at your size) | 50,000 EUR (irrelevant) |
| Fee gotcha | **1 free withdrawal/week, then 4% fee** | KYC delays widely reported |
| KYC | ID + proof of address; verification before/at withdrawal | ID + proof of address; reports of **4–6 days up to 1 month** |

### Step-by-step

1. **Complete KYC first, before requesting any withdrawal.** This is the single biggest delay/risk point (CoolBet verification can take days to a month). Upload a clear passport/national-ID + a proof-of-address (bank statement / utility bill with name + address) on each site *now*, and wait for "verified" status before requesting cash-out.
2. **Use bank transfer / IBAN to an Icelandic account** (the user's stated preferred route), not crypto. Crypto cash-out can pull winnings into Iceland's harsher crypto tax bucket (industry sources cite a flat ~38.5%) and adds a conversion leg. IBAN keeps it in EUR → your ISK account.
3. **Withdraw each balance in a single request** to avoid EpicBet's "1 free withdrawal/week, then 4% fee" — i.e. pull the full 595 EUR in **one** EpicBet request, the full 541 EUR in **one** CoolBet request. At 595 EUR, a 4% fee is ~24 EUR; one-shot withdrawals avoid it entirely.
4. **Withdrawal = same method as deposit** where the operator enforces it (both tend to). If you originally deposited by card/e-wallet, you may have to withdraw a matching portion that way first; check the cashier screen — the available methods are gated on deposit history.
5. **Expect a 0–72h pending hold then 1–5 business days** to land in the Icelandic account. Budget ~1–2 weeks end-to-end including KYC.
6. **Currency:** funds are EUR. Your bank converts to ISK at receipt; at ~1,136 EUR this is a modest amount, unlikely to trip large-transfer AML thresholds, but Icelandic banks are obliged to report suspicious/significant inbound transfers — keep the operator's withdrawal confirmation as provenance.

### Tax / reporting (do this deliberately)

- **All foreign-source amounts must be reported on the framtal** (RSK §2.8 Tekjur erlendis), even if you'll argue they're exempt.
- **CoolBet (EEA):** you *may* be able to claim the EEA exemption (RSK §8.4.2) — but only *with documentation about the operator*; it's framed for lotteries, so keep records and ideally get a professional opinion that a Malta sportsbook qualifies.
- **EpicBet (non-EEA Curaçao):** the EEA-lottery exemption almost certainly does **not** apply; treat as a **declarable foreign windfall** unless a professional says otherwise.
- **Keep:** deposit/withdrawal histories, net-position statements, and the IBAN transfer confirmations. If RSK queries the inbound transfer, you want to show net winnings + operator identity, not just a bare bank credit.

### Timing

Do the extraction **promptly** (this season). The legislation reform is operator-focused and not imminent, but the realistic downside risk to *you* — future payment/transaction blocking or tighter AML scrutiny on unlicensed-operator inflows — only makes withdrawals harder over time, never easier.

### After extraction

Leave both accounts open but **dormant** (legal to hold; no automation). Reassess only if the reform actually licenses these operators in Iceland (which would *improve*, not worsen, the picture).

---

# Appendix B — Legality & tax findings (detail)

## Icelandic online-betting legality — findings (with sources)

**NOT legal/tax advice — confirm with an Icelandic lawyer/accountant (lögfræðingur/endurskoðandi) before acting on money.**

### 1. What is strictly legal vs the foreign books

Iceland runs a **state/charity gambling monopoly**, largely unchanged since ~2005. Only a small set of licensed domestic entities may *operate* gambling:
- **Íslensk getspá / Íslenskar getraunir** (the entity behind **Lengjan** and **lotto.is**) — the only licensed sports-betting/lottery operator for locals.
- Other licensed lotteries: HHÍ (University of Iceland lottery), DAS, Íslandsspil, SÍBS, and various charity lotteries.

There is **no domestic licence** for private online operators. **EpicBet** (Comeback N.V., licensed in **Curaçao** + Estonia + Anjouan) and **CoolBet** (Polar Limited / licensed in **Malta, Estonia, Sweden**) operate from abroad and are **not licensed to operate inside Iceland**.

### 2. Is it illegal for a *resident* to bet on / hold funds at foreign books?

The prohibition targets **operators, not individual punters**. Multiple sources agree: there is **no law penalising an Icelandic resident for accessing or using a foreign bookmaker**, authorities have little jurisdiction over the sites, and punters are **not prosecuted**. CoolBet explicitly accepts Icelandic players with Icelandic-language support and EUR; EpicBet supports Icelandic and operates in EUR. The user-facing risk is **no domestic regulatory protection** (no Icelandic recourse if a foreign book withholds funds), not criminal exposure for the bettor. *(This is the consensus of gambling-industry/legal-overview sites, not a statute citation — confirm with a professional.)*

### 3. Tax treatment of gambling winnings (the one genuinely nuanced area)

Iceland's tax rule (RSK/Skatturinn, leiðbeiningar §8.4.2 *Skattfrjálsir vinningar*) is **operator-specific, not "all gambling is tax-free":**
- **Tax-free (skattfrjálsir vinningar):** winnings from the named licensed Icelandic operators — explicitly **including Icelandic betting pools/sports betting (getraunir/Lengjan)** and the listed lotteries. So **Lengjan winnings are tax-free.**
- **Foreign winnings:** "Lottery winnings **from the EEA** may be tax-free **in the same manner**, *provided* the taxpayer submits adequate documentation about the lottery" — RSK applies identical requirements to foreign and domestic lotteries. Critically, this exemption is written for **happdrætti (lotteries within the EEA)**. EpicBet is **Curaçao-licensed (non-EEA)**; CoolBet is **Malta/Estonia-licensed (EEA)** but is a *sportsbook/casino*, not a "lottery (happdrætti)" in the §8.4.2 sense. Whether a Malta-licensed sportsbook's winnings clear the EEA-lottery exemption is **genuinely unclear and document-dependent** — RSK guidance frames it around lotteries.
- **General rule (RSK):** winnings from betting/lotteries/raffles are **taxable unless specifically exempted**, and **all foreign-source income earned while resident in Iceland must be reported** on the framtal. Taxable "other income" runs personal-income rates (~17–46% banded). (One industry source even claims crypto-routed winnings get lumped into a flat 38.5% — relevant only if you cash out via crypto rather than IBAN.)
- **Bottom line for tax:** Lengjan = clearly tax-free. CoolBet (EEA) = plausibly exemptible *with documentation*, but not certain — it hinges on whether RSK treats a sportsbook as a "happdrætti." EpicBet (non-EEA Curaçao) = **weakest exemption case**; safest assumption is a **declarable foreign windfall**. **Get a professional ruling before relying on tax-free treatment of either foreign book.**

### 4. The talked-about legislation changes (status)

- A reform push exists but **no bill has been enacted, and as of 2025–2026 there is no live bill before Alþingi with a concrete timeline.** Efforts have "repeatedly faltered" over ~20 years; a government task force three years ago reached no consensus.
- **Direction (if it ever passes):** *liberalising*, not prohibitionist. The **Icelandic Chamber of Commerce (Viðskiptaráð)** proposes scrapping the exclusive-licence monopoly for a **licensing regime open to domestic *and international* operators** (~22% duty on operator revenue, projected ~4.8 bn ISK/yr; estimates of ~20 bn ISK/yr wagered abroad, ~10 bn ISK leaving the economy). Justice Minister **Þorbjörg Sigríður Gunnlaugsdóttir** has signalled openness; ÍSÍ's president called for a comprehensive review; a Nordic-style "one-card" unified player-tracking model is floated.
- **Impact on the user's situation:** the reform targets **operators**, not punters. No source describes any proposal to **penalise individuals holding/withdrawing foreign-book funds** or to claw back existing balances. The realistic risk vectors are **indirect**: (a) future **payment/transaction blocking** (banks/PSPs barred from processing unlicensed-operator flows — common in regulated markets), which could make *future* IBAN withdrawals harder; (b) tighter **AML/KYC bank reporting** making large inbound transfers more scrutinised. Neither is enacted; both argue for **withdrawing sooner rather than later** if extraction is the goal.

### 5. Scraper feasibility (EpicBet & CoolBet)

- **CoolBet:** site sits behind **Cloudflare bot protection** — a direct fetch of `coolbet.com/en/help/payment-methods` returned a Cloudflare "you look like a bot" wall. A naive headless Playwright/Chromote script "gets blocked in seconds" (TLS fingerprinting + JS challenge + Turnstile). Bypassing needs stealth plugins, residential/rotating proxies, persistent authenticated sessions, possibly a CAPTCHA solver — **materially harder than the existing Lengjan scraper, which is *not* Cloudflare-walled.** There is **no public CoolBet odds API**; the only "CoolBet API" hits are **paid third-party aggregators (OpticOdds)**, not a first-party endpoint.
- **EpicBet:** smaller Curaçao brand, less documented; no public API. Same anti-bot expectation for an authenticated balance scraper.
- **Project fit:** the repo already has a **mature Chromote browser-automation stack** (`R/ingest-lengjan-odds.R`, `R/placer-login.R`, `R/placer-navigate.R`, `R/ingest.R`), so the *plumbing* to log in and read a balance/odds page exists. The blocker is **Cloudflare on the targets**, not missing tooling. A **balance-only** scraper (log in, read one number, log out, on a slow manual cadence) is far more tractable and lower-detection-risk than a high-frequency **odds** scraper (which would hammer the site and trip rate/behavioural detection fast).

**Sources:**
- [cheekypunter.com — Online Betting in Iceland: Legality](https://www.cheekypunter.com/country/iceland/)
- [legalpilot.com — Is Gambling Legal in Iceland? 2026](https://legalpilot.com/country/iceland/)
- [Skatturinn / RSK leiðbeiningar §8.4.2 Skattfrjálsir vinningar](https://leidbeiningar.rsk.is/frodi/?cat=1562&id=21646&k=8)
- [RSK §2.8 Tekjur erlendis (foreign income)](https://leidbeiningar.rsk.is/frodi/?cat=1342&id=19125&k=2)
- [igamingtoday.com — Iceland poised for gambling reform](https://www.igamingtoday.com/iceland-poised-for-gambling-reform-government-considers-opening-the-market/)
- [igamingtoday.com — Reforming Iceland's Gambling Legislation](https://www.igamingtoday.com/reforming-icelands-gambling-legislation/)
- [focusgn.com — Iceland's Parliament pressed to overhaul gambling oversight](https://focusgn.com/icelands-parliament-pressed-to-overhaul-gambling-oversight)
- [casino.guru — CoolBet review (licences, KYC, limits)](https://casino.guru/CoolBet-Casino-review)
- [askgamblers.com — EpicBet Sportsbook review (licences, withdrawals, KYC)](https://www.askgamblers.com/sports-betting/sportsbook-reviews/epicbet-sportsbook)
- [coolbet.com payment-methods (returned Cloudflare bot-wall)](https://www.coolbet.com/en/help/payment-methods)
- [zenrows.com — Bypass Cloudflare with Playwright (anti-bot difficulty)](https://www.zenrows.com/blog/playwright-cloudflare-bypass)
- [opticodds.com — Coolbet API (3rd-party aggregator, not first-party)](https://opticodds.com/sportsbooks/coolbet-api)

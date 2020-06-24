# ツール連携について

PSDToolKit を使う際には、音声合成ソフトと組み合わせて使うことがしばしばあると思います。

Shift キーを押しながら音声ファイルをドロップすることで `口パク準備` や `字幕準備`、`多目的スライダー` などが自動生成されるというのは[チュートリアル](tutorial.md)にて紹介しましたが、設定をきちんと行うとソフトによっては音声ファイルを保存するだけでカーソル位置に自動で挿入することができるようになるものまであります。

ここでは[チュートリアル](tutorial.md)を既に読み終わり PSDToolKit についての基本的な知識が既に備わっている方々へ向けて、ツール連携方法について紹介していきます。

# ざっくりフローチャート

あなたのニーズに合わせて何を用意すればいいかザックリと導くフローチャートです。  
最後の項目はリンクになっており、クリックすると詳細な説明に飛べます。

<svg fill="none" stroke-linecap="square" stroke-miterlimit="10" version="1.1" viewBox="0 0 1060 574" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<clipPath>
<path d="m0 0h1060v574h-1060v-574z"/>
</clipPath>
<path d="m0 0h1060v574h-1060z" fill-opacity="0"/>
<path d="m591 259h26" fill-opacity="0"/>
<path d="m591 259h20" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m611 261 4.54-1.65-4.54-1.65z" fill="#000" fill-rule="evenodd" stroke="#000" stroke-linecap="butt"/>
<path d="m750 288c0 13.9 39.3 20.9 78.5 27.9 39.3 6.96 78.5 13.9 78.5 27.9" fill-opacity="0"/>
<path d="m750 288c0 13.9 39.3 20.9 78.5 27.9 19.6 3.48 39.3 6.96 54 11.3 7.36 2.18 13.5 4.57 17.8 7.29 1.07 0.68 2.03 1.38 2.87 2.1 0.417 0.361 0.803 0.728 1.16 1.1 0.0883 0.0931 0.175 0.187 0.259 0.281l0.187 0.215" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m903 339 3.25 3.57-0.196-4.83z" fill="#000" fill-rule="evenodd" stroke="#000" stroke-linecap="butt"/>
<path d="m750 288c0 33.3-32.2 51.4-61.9 66.6s-57 27.5-61.9 47.5c-4.93 19.9 12.5 47.5 25 47.5" fill-opacity="0"/>
<path d="m750 288c0 33.3-32.2 51.4-61.9 66.6s-57 27.5-61.9 47.5c-2.46 9.97 0.661 21.8 5.96 31.2 2.65 4.69 5.84 8.75 9.16 11.6 0.828 0.723 1.66 1.37 2.5 1.94 0.418 0.284 0.837 0.548 1.25 0.79 0.104 0.0606 0.209 0.12 0.313 0.178l0.0408 0.0221" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m645 449 4.82-0.312-3.89-2.86z" fill="#000" fill-rule="evenodd" stroke="#000" stroke-linecap="butt"/>
<path d="m360 259h-26" fill-opacity="0"/>
<path d="m360 259h-20" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m340 258-4.54 1.65 4.54 1.65z" fill="#000" fill-rule="evenodd" stroke="#000" stroke-linecap="butt"/>
<path d="m273 288c0 13.9-13.3 20.9-26.7 27.9-13.3 6.96-26.7 13.9-26.7 27.9" fill-opacity="0"/>
<path d="m273 288c0 13.9-13.3 20.9-26.7 27.9-6.67 3.48-13.3 6.96-18.3 11.3-2.5 2.18-4.58 4.57-6.04 7.29-0.364 0.68-0.69 1.38-0.973 2.1-0.142 0.361-0.273 0.728-0.393 1.1l-0.0352 0.112" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m219 337 0.96 4.73 2.31-4.24z" fill="#000" fill-rule="evenodd" stroke="#000" stroke-linecap="butt"/>
<path d="m475 302v24.5" fill-opacity="0"/>
<path d="m475 302v18.5" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m474 321 1.65 4.54 1.65-4.54z" fill="#000" fill-rule="evenodd" stroke="#000" stroke-linecap="butt"/>
<path d="m475 384c0 28.7-65 43.1-130 57.4s-130 28.7-130 57.4" fill-opacity="0"/>
<path d="m475 384c0 28.7-65 43.1-130 57.4-32.5 7.18-65 14.4-89.4 23.3-12.2 4.49-22.3 9.42-29.4 15-3.55 2.8-6.35 5.78-8.25 8.94-0.476 0.792-0.896 1.6-1.26 2.41-0.181 0.408-0.349 0.82-0.501 1.24-0.0381 0.104-0.0753 0.208-0.112 0.312l-0.081 0.239" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m215 492 0.899 4.74 2.36-4.21z" fill="#000" fill-rule="evenodd" stroke="#000" stroke-linecap="butt"/>
<path d="m472 147c-17.2 0-32.4 15.4-34.4 28-2.05 12.6 9.03 22.5 19.1 28 10.1 5.53 19.1 6.73 19.1 13.5" fill-opacity="0"/>
<path d="m472 147c-17.2 0-32.4 15.4-34.4 28-2.05 12.6 9.03 22.5 19.1 28 5.03 2.76 9.8 4.45 13.3 6.28 0.878 0.458 1.68 0.925 2.38 1.42 0.176 0.124 0.347 0.25 0.511 0.378l0.0853 0.0682" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m471 212 3.57 3.25-0.648-4.79z" fill="#000" fill-rule="evenodd" stroke="#000" stroke-linecap="butt"/>
<path d="m604 63.1v36.9" fill-opacity="0"/>
<path d="m604 63.1v30.9" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m602 93.9 1.65 4.54 1.65-4.54z" fill="#000" fill-rule="evenodd" stroke="#000" stroke-linecap="butt"/>
<g fill-opacity="0">
<path d="m380 155h78.6v45h-78.6z"/>
<path d="m703 97.7h78.6v45h-78.6z"/>
<path d="m736 147c21.2 0 31.8-6.66 42.5-13.3 10.6-6.66 21.2-13.3 42.5-13.3"/>
</g>
<path d="m736 147c21.2 0 31.8-6.66 42.5-13.3 5.31-3.33 10.6-6.66 17.2-9.16 3.32-1.25 6.96-2.29 11.1-3.02 2.07-0.364 4.27-0.651 6.61-0.846 0.293-0.0244 0.588-0.0474 0.885-0.0689l0.601-0.0402" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m815 122 4.48-1.79-4.59-1.51z" fill="#000" fill-rule="evenodd" stroke="#000" stroke-linecap="butt"/>
<path d="m475 384c0 28.7 36.8 43.1 73.5 57.4 36.8 14.4 73.5 28.7 73.5 57.4" fill-opacity="0"/>
<path d="m475 384c0 28.7 36.8 43.1 73.5 57.4 18.4 7.18 36.8 14.4 50.5 23.3 6.89 4.49 12.6 9.42 16.7 15 2.01 2.8 3.59 5.78 4.67 8.94 0.269 0.792 0.507 1.6 0.712 2.41 0.103 0.408 0.197 0.82 0.283 1.24l0.0983 0.498" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m620 493 2.07 4.37 1.22-4.67z" fill="#000" fill-rule="evenodd" stroke="#000" stroke-linecap="butt"/>
<a xlink:href="#.E5.A5.BD.E3.81.8D.E3.81.AA.E3.82.82.E3.81.AE.E4.BD.BF.E3.81.A3.E3.81.A6.E3.81.AD">
<path d="m930 94.6 37.7-53.1-3.72 48.8 42.8-7.95-17 26.2 44.5 7.53-35.3 21.4 40.4 25.8-48.2-3.19 13.2 47.2-41.8-33.3-7.83 48.4-27.5-44-20.7 61.1-7.79-54.7-29.9 18.2 9.16-33.7-56 5.96 36.3-25.8-37.7-29 46.8-9.14-43.1-48.8 70.3 36.9 10.5-36.9z" fill="#f00" fill-rule="evenodd"/>
<path d="m930 94.6 37.7-53.1-3.72 48.8 42.8-7.95-17 26.2 44.5 7.53-35.3 21.4 40.4 25.8-48.2-3.19 13.2 47.2-41.8-33.3-7.83 48.4-27.5-44-20.7 61.1-7.79-54.7-29.9 18.2 9.16-33.7-56 5.96 36.3-25.8-37.7-29 46.8-9.14-43.1-48.8 70.3 36.9 10.5-36.9z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<text x="930.07318" y="136.88858" fill="#ffffff" font-family="sans-serif" font-size="18.7px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="930.07318" y="136.88858">好きなもの</tspan><tspan x="930.07318" y="157.41463">使ってね</tspan></text>
</a>
<path d="m472 15.8c0-5.22 4.23-9.45 9.45-9.45h245c2.51 0 4.91 0.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-245c-5.22 0-9.45-4.23-9.45-9.45z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m472 15.8c0-5.22 4.23-9.45 9.45-9.45h245c2.51 0 4.91 0.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-245c-5.22 0-9.45-4.23-9.45-9.45z" fill="#4a86e8" fill-rule="evenodd"/>
<text x="604.3009" y="41.485973" fill="#ffffff" font-family="sans-serif" font-size="18.7px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="604.3009" y="41.485973">スタート</tspan></text>
<path d="m472 147 132-47.1 132 47.1-132 47.1z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m472 147 132-47.1 132 47.1-132 47.1z" fill="#d9ead3" fill-rule="evenodd"/>
<text x="604.01056" y="141.62169" fill="#000000" font-family="sans-serif" font-size="18.7px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="604.01056" y="141.62169" text-align="center" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">音声を自動で</tspan><tspan x="604.01056" y="166.09564" text-align="center" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">挿入したい</tspan></text>
<path d="m360 259 115-42.9 115 42.9-115 42.9z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m360 259 115-42.9 115 42.9-115 42.9z" fill="#d9ead3" fill-rule="evenodd"/>
<text x="475.65997" y="254.23415" fill="#000000" font-family="sans-serif" font-size="18.7px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="475.65997" y="254.23415" text-align="center" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">使いたい</tspan><tspan x="475.65997" y="278.7081" text-align="center" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">ソフトは</tspan></text>
<path d="m212 231c0-5.6e-4 4.6e-4 -1e-3 1e-3 -1e-3l123 1e-3c2.7e-4 0 5.2e-4 1.1e-4 7e-4 3.1e-4 2.1e-4 1.9e-4 3e-4 4.5e-4 3e-4 7.1e-4l-1e-3 56.7c0 5.5e-4 -4.6e-4 1e-3 -1e-3 1e-3l-123-1e-3c-5.6e-4 0-1e-3 -4.6e-4 -1e-3 -1e-3z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<path d="m212 231c0-5.6e-4 4.6e-4 -1e-3 1e-3 -1e-3l123 1e-3c2.7e-4 0 5.2e-4 1.1e-4 7e-4 3.1e-4 2.1e-4 1.9e-4 3e-4 4.5e-4 3e-4 7.1e-4l-1e-3 56.7c0 5.5e-4 -4.6e-4 1e-3 -1e-3 1e-3l-123-1e-3c-5.6e-4 0-1e-3 -4.6e-4 -1e-3 -1e-3z" fill="#cfe2f3" fill-rule="evenodd"/>
<text x="272.94882" y="266.24637" fill="#000000" font-family="sans-serif" font-size="24px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="272.94882" y="266.24637" font-family="sans-serif" font-size="18.7px" text-align="center" text-anchor="middle" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">CeVIO</tspan></text>
<a xlink:href="#.E3.81.8B.E3.82.93.E3.81.97.E3.81.8F.E3.82.93_.2B_CeVIO_Creative_Studio">
<path d="m50.2 353c0-5.22 4.23-9.45 9.45-9.45h320c2.51 0 4.91 0.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-320c-5.22 0-9.45-4.23-9.45-9.45z" fill="#cfe2f3" fill-rule="evenodd"/>
<path d="m50.2 353c0-5.22 4.23-9.45 9.45-9.45h320c2.51 0 4.91 0.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-320c-5.22 0-9.45-4.23-9.45-9.45z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<text x="219.51993" y="378.78302" fill="#000000" font-family="sans-serif" font-size="24px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="219.51993" y="378.78302" font-family="sans-serif" font-size="18.7px" text-align="center" text-anchor="middle" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">かんしくん + CeVIO Creative Studio</tspan></text>
</a>
<path d="m414 327c0-5.8e-4 4.6e-4 -1e-3 1e-3 -1e-3l123 1e-3c3e-4 0 5.5e-4 9e-5 7.3e-4 3.1e-4 1.8e-4 1.8e-4 3.1e-4 4.2e-4 3.1e-4 7e-4l-1e-3 56.7c0 5.8e-4 -4.3e-4 1e-3 -9.8e-4 1e-3l-123-1e-3c-5.5e-4 0-1e-3 -4.6e-4 -1e-3 -1e-3z" fill="#f4cccc" fill-rule="evenodd"/>
<path d="m414 327c0-5.8e-4 4.6e-4 -1e-3 1e-3 -1e-3l123 1e-3c3e-4 0 5.5e-4 9e-5 7.3e-4 3.1e-4 1.8e-4 1.8e-4 3.1e-4 4.2e-4 3.1e-4 7e-4l-1e-3 56.7c0 5.8e-4 -4.3e-4 1e-3 -9.8e-4 1e-3l-123-1e-3c-5.5e-4 0-1e-3 -4.6e-4 -1e-3 -1e-3z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<text x="477.09851" y="361.98758" fill="#000000" font-family="sans-serif" font-size="24px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="477.09851" y="361.98758" font-family="sans-serif" font-size="18.7px" text-align="center" text-anchor="middle" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">ゆっくり</tspan></text>
<path d="m617 231c0-1e-3 9.8e-4 -2e-3 2e-3 -2e-3l267 2e-3c6.1e-4 0 1e-3 2.4e-4 2e-3 6.6e-4 4.2e-4 4.1e-4 6.7e-4 9.9e-4 6.7e-4 2e-3l-2e-3 56.7c0 1e-3 -9.8e-4 2e-3 -2e-3 2e-3l-267-2e-3c-1e-3 0-2e-3 -1e-3 -2e-3 -2e-3z" fill="#d9d2e9" fill-rule="evenodd"/>
<path d="m617 231c0-1e-3 9.8e-4 -2e-3 2e-3 -2e-3l267 2e-3c6.1e-4 0 1e-3 2.4e-4 2e-3 6.6e-4 4.2e-4 4.1e-4 6.7e-4 9.9e-4 6.7e-4 2e-3l-2e-3 56.7c0 1e-3 -9.8e-4 2e-3 -2e-3 2e-3l-267-2e-3c-1e-3 0-2e-3 -1e-3 -2e-3 -2e-3z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<text x="750.80328" y="266.19644" fill="#000000" font-family="sans-serif" font-size="24px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="750.80328" y="266.19644" font-family="sans-serif" font-size="18.7px" text-align="center" text-anchor="middle" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">VOICEROID などの AITalk 系</tspan></text>
<a xlink:href="#VoiceroidUtil_.2B_AITalk_.E7.B3.BB">
<path d="m775 353c0-5.22 4.23-9.45 9.45-9.45h245c2.51 0 4.91 0.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-245c-5.22 0-9.45-4.23-9.45-9.45z" fill="#d9d2e9" fill-rule="evenodd"/>
<path d="m775 353c0-5.22 4.23-9.45 9.45-9.45h245c2.51 0 4.91 0.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-245c-5.22 0-9.45-4.23-9.45-9.45z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<text x="907.80804" y="378.59943" fill="#000000" font-family="sans-serif" font-size="24px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="907.80804" y="378.59943" font-family="sans-serif" font-size="18.7px" text-align="center" text-anchor="middle" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">VoiceroidUtil + AITalk 系</tspan></text>
</a>
<a xlink:href="#.E3.81.8B.E3.82.93.E3.81.97.E3.81.8F.E3.82.93_.2B_VoiceroidUtil_.2B_AITalk_.E7.B3.BB">
<path d="m651 430c0-5.22 4.23-9.45 9.45-9.45h369c2.51 0 4.91 0.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-369c-5.22 0-9.45-4.23-9.45-9.45z" fill="#d9d2e9" fill-rule="evenodd"/>
<path d="m651 430c0-5.22 4.23-9.45 9.45-9.45h369c2.51 0 4.91 0.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-369c-5.22 0-9.45-4.23-9.45-9.45z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<text x="845.29266" y="456.05347" fill="#000000" font-family="sans-serif" font-size="24px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="845.29266" y="456.05347" font-family="sans-serif" font-size="18.7px" text-align="center" text-anchor="middle" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">かんしくん + VoiceroidUtil + AITalk 系</tspan></text>
</a>
<a xlink:href="#.E3.81.8B.E3.82.93.E3.81.97.E3.81.8F.E3.82.93_.2B_.E6.A3.92.E8.AA.AD.E3.81.BF.E3.81.A1.E3.82.83.E3.82.93_.2B_Plugin_Bouyomichan_wav_and_txt.dll">
<path d="m425 508c0-5.22 4.23-9.45 9.45-9.45h375c2.51 0 4.91 0.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-375c-5.22 0-9.45-4.23-9.45-9.45z" fill="#f4cccc" fill-rule="evenodd"/>
<path d="m425 508c0-5.22 4.23-9.45 9.45-9.45h375c2.51 0 4.91 0.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-375c-5.22 0-9.45-4.23-9.45-9.45z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<text x="622.47394" y="519.45349" fill="#000000" font-family="sans-serif" font-size="18.7px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="622.47394" y="519.45349" text-align="center" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">かんしくん + 棒読みちゃん +</tspan><tspan x="622.47394" y="544.83893" text-align="center" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">Plugin_Bouyomichan_wav_and_txt.dll</tspan></text>
</a>
<a xlink:href="#.E3.81.8B.E3.82.93.E3.81.97.E3.81.8F.E3.82.93_.2B_.E3.82.86.E3.81.A3.E3.81.8F.E3.82.8A.E8.AA.BF.E5.A3.B0.E3.81.8F.E3.82.93_.2B_SofTalk">
<path d="m19 508c0-5.22 4.23-9.45 9.45-9.45h374c2.51 0 4.91 0.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-374c-5.22 0-9.45-4.23-9.45-9.45z" fill="#f4cccc" fill-rule="evenodd"/>
<path d="m19 508c0-5.22 4.23-9.45 9.45-9.45h374c2.51 0 4.91 0.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-374c-5.22 0-9.45-4.23-9.45-9.45z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/>
<text x="214.95998" y="533.47241" fill="#000000" font-family="sans-serif" font-size="24px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="214.95998" y="533.47241" font-family="sans-serif" font-size="18.7px" text-align="center" text-anchor="middle" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">かんしくん + ゆっくり調声くん + SofTalk</tspan></text>
</a>
<text x="408.6369" y="181.85616" fill="#000000" font-family="sans-serif" font-size="24px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="408.6369" y="181.85616" font-family="sans-serif" font-size="18.7px" text-align="center" text-anchor="middle" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">はい</tspan></text>
<text x="766.9043" y="167.35141" fill="#000000" font-family="sans-serif" font-size="24px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:100%" xml:space="preserve"><tspan x="766.9043" y="167.35141" font-family="sans-serif" font-size="18.7px" text-align="center" text-anchor="middle" style="font-feature-settings:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal">いいえ</tspan></text>
</svg>

音声ファイルの自動挿入には[ごちゃまぜドロップス](https://github.com/oov/aviutl_gcmzdrops)が持つ [外部連携 API](https://github.com/oov/aviutl_gcmzdrops#%E5%A4%96%E9%83%A8%E9%80%A3%E6%90%BA-api-%E3%81%AB%E3%81%A4%E3%81%84%E3%81%A6) を使う必要があり、  
これに対応しているのは現時点では [VoiceroidUtil](https://www.nicovideo.jp/watch/sm28526328) と拙作の [かんしくん](#かんしくん) だけです。

そのため自動挿入するには `VoiceroidUtil` を使うか、音声とテキストが同時に保存できるソフトと `かんしくん` を組み合わせて使うかの２択になります。  
後者のパターンは上記フローチャートにはない組み合わせで動くものもたくさんありますが、ここでは一番ニーズが多そうなものに絞って紹介しています。

## 好きなもの使ってね

自動挿入を使わないのであれば、音声ファイルさえあれば使えます。  
音声ファイルを生成できるソフトと自由に組み合わせてお使いください。

[チュートリアルで紹介](tutorial.md#口パク準備と字幕準備の配置を効率化)したように設定を行えば、ファイルを拡張編集に投げ込むときに Shift キーを押しておくことによって口パク準備などを自動生成できますし、テキストファイルがあれば字幕準備の自動生成もできます。

また、ソフトによっては `*.exo` ファイルが生成できるものもあります。  
音声とテキストのみが入った `*.exo` を使うなら、PSDToolKit の [環境設定ダイアログ](setting.md#環境設定ダイアログ) にある `1フレーム目に音声とテキストがある *.exo をドロップした時` にチェックを入れておくことで、挿入されるデータを `字幕準備`や `口パク準備` などに変化させることもできます。

## VoiceroidUtil + AITalk 系

[VoiceroidUtil](https://www.nicovideo.jp/watch/sm28526328) は VOICEROID などの AITalk 系製品の起動／終了を管理したり、規則的なファイル名での保存や、外部ツールとの連携をサポートしてくれます。

VoiceroidUtil を使えば保存のたびに `名前をつけて保存` のダイアログを使う必要がなくなり、更に連携設定を行えば拡張編集への自動挿入まで行えるようになります。

### VoiceroidUtil の初回起動から連携設定までの最短ルート

1. VoiceroidUtil を起動します。
2. `設定` タブをクリックします。
3. `音声保存` タブをクリックします。
4. `AviUtl拡張編集ファイル(.exo)を作成` にチェックを入れます。

これで VoiceroidUtil での音声ファイル保存時に AviUtl にテキストと音声が挿入されるようになりました。  
ただし、この状態で挿入されるのは普通のテキストオブジェクトと音声です。

ここから更に PSDToolKit の [環境設定ダイアログ](setting.md#環境設定ダイアログ) にある `1フレーム目に音声とテキストがある *.exo をドロップした時` にチェックを入れておくことで、挿入されるデータを `字幕準備`や `口パク準備` などに変化させることができます。

なお PSDToolKit で `字幕準備` や `口パク準備` などを作成するようにすると VoiceroidUtil 側で設定したキャラごとのフォント設定などはすべて無効になりますが、これは仕様です。  
PSDToolKit ではフォント設定などは `字幕表示` で行います。

## かんしくん + VoiceroidUtil + AITalk 系

[VoiceroidUtil のみを使って自動挿入する](#VoiceroidUtil_+_AITalk_系)こともできますが、[かんしくん](#かんしくん)を組み合わせて使うこともできます。  
色々な音声合成ソフトを組み合わせて使う場合にはこの構成が選ばれることもあります。

この構成で使用するなら VoiceroidUtil はデフォルト設定のまま使うことができます。

### かんしくんの設定例

かんしくんの基本的な部分については[こちら](forcepser.md)を参照ください。  
VoiceroidUtil がすべてデフォルト設定なら以下のような設定で動作します。  
（`basedir` は不使用）

```txt
delta = 15.0
freshness = 5.0

[[rule]]
dir = 'C:\Users\ユーザー名\Documents\VoiceroidWaveFiles'
file = '*_東北きりたん_*.wav'
encoding = 'sjis'
layer = 1
```

VOICEROID2 や ガイノイドTalk などでボイスプリセット名付きで発言させる場合（例えば `鳴花ヒメ＞こんにちは` のような使い方）は、以下のようにテキストファイル内のボイスプリセット名を基準に判定させることもできます。

```txt
delta = 15.0
freshness = 5.0

[[rule]]
dir = 'C:\Users\ユーザー名\Documents\VoiceroidWaveFiles'
file = '*.wav' # ファイル名は何でもいい
text = '''^鳴花ヒメ＞'''
encoding = 'sjis'
layer = 1
modifier = '''text = re.gsub(text, "^.*?＞", "")''' # テキストの冒頭のボイスプリセット指定を除去
```

## かんしくん + 棒読みちゃん + Plugin_Bouyomichan_wav_and_txt.dll

[棒読みちゃん](https://chi.usamimi.info/Program/Application/BouyomiChan/) に [Plugin_Bouyomichan_wav_and_txt.dll](https://github.com/yukimaru73/Plugin_Bouyomichan_wav_and_txt) を導入すると、音声ファイルとテキストファイルを同時に保存できるようになります。

### プラグインを導入済みの棒読みちゃんの初回起動から連携設定までの最短ルート

1. 棒読みちゃんを起動します。
2. `音声 テキスト出力` プラグインを有効にします。  
（※それ以外のプラグインは連携に必要ありません）
3. `その他` タブをクリックします。
4. `音声 テキスト出力` を選択し、下にあるスパナアイコンをクリックします。
5. ファイルの保存場所を好きな場所に変更します。  
（例えば `C:\Users\ユーザー名\Documents\BouyomiChan` など）

これで `音声合成` タブに戻り、セリフを入力してツールバー右上のほうにある `🛑/📝` のようなボタンを押すと音声とテキストの保存ができます。

### かんしくんの設定例

かんしくんの基本的な部分については[こちら](forcepser.md)を参照ください。  
保存先を `C:\Users\ユーザー名\Documents\BouyomiChan` に設定している場合、以下のような設定で動作します。  
（`basedir` は不使用）

```txt
delta = 15.0
freshness = 5.0

[[rule]]
dir = 'C:\Users\ユーザー名\Documents\BouyomiChan'
file = '*_女性１_*.wav'
encoding = 'sjis'
layer = 1
```

## かんしくん + ゆっくり調声くん + SofTalk

[SofTalk](https://w.atwiki.jp/softalk/pages/1.html) はそれ単体でも音声とテキストファイルを保存することができますが、[ゆっくり調声くん](https://www.nicovideo.jp/watch/sm32835672)と組み合わせると詳細な調声やセリフの再編集なども行いやすくなります。

### ゆっくり調声くん + SofTalk の初回起動から連携設定までの最短ルート

1. ゆっくり調声くんを起動します。
2. `設定` タブをクリックします。
3. 一番下にある `SofTalk.exe ファイルパス` に SofTalk.exe のファイルパスを指定します。
4. `音声作成` タブをクリックします。
5. `ファイル` メニューから `新規プロジェクト開始` を選びます。
6. プロジェクトのフォルダーを選択します。  
（例えば `C:\Users\ユーザー名\Documents\YukkuriChoseiKun` など）

これで左上の `新規作成` を選び、緑の枠の一番上の入力欄にセリフを入力して `変換` ボタンを押すと読み上げに関する設定画面が現れます。  
この状態で下の `挿入` ボタン押すと上のセリフ一覧にセリフが追加され、それと同時に音声とテキストの保存がされます。

### かんしくんの設定例

かんしくんの基本的な部分については[こちら](forcepser.md)を参照ください。  
保存先を `C:\Users\ユーザー名\Documents\YukkuriChoseiKun` に設定している場合、以下のような設定で動作します。  
（`basedir` は不使用）

```txt
delta = 15.0
freshness = 5.0

[[rule]]
dir = 'C:\Users\ユーザー名\Documents\YukkuriChoseiKun'
file = '*_霊夢_*.wav'
encoding = 'sjis'
layer = 1
```

## かんしくん + CeVIO Creative Studio

[CeVIO Creative Studio](http://cevio.jp/) では保存時にテキストも同時に保存することができます。

CeVIO Creative Studio での事前の設定は特に必要なく、トークトラックに適当にセリフを入力し、`Ctrl + W` を押すか、右クリックメニューから `WAV書き出し(W)...` を選び、出てきた保存ダイアログの下にある `セリフをテキスト出力` にチェックを付けた状態で音声ファイルを保存することで、音声とテキストが同時に保存されます。

### かんしくんの設定例

かんしくんの基本的な部分については[こちら](forcepser.md)を参照ください。  
音声ファイルを `C:\Users\ユーザー名\Documents\CeVIO` に保存する場合、以下のような設定で動作します。  
（`basedir` は不使用）

なお識別にファイル名を利用しているため、ファイル名はデフォルトのままで保存してください。

```txt
delta = 15.0
freshness = 5.0

[[rule]]
dir = 'C:\Users\ユーザー名\Documents\CeVIO'
file = '*_さとうささら_*.wav'
encoding = 'sjis'
layer = 1
```
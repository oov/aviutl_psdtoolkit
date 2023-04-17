# ツール連携について

PSDToolKit を使う際には、音声合成ソフトと組み合わせて使うことがしばしばあると思います。

Shift キーを押しながら音声ファイルをドロップすることで `口パク準備` や `字幕準備`、`多目的スライダー` などが自動生成されるというのは[チュートリアル](tutorial.md)にて紹介しましたが、設定をきちんと行うとソフトによっては音声ファイルを保存するだけでカーソル位置に自動で挿入することができるようになるものまであります。

ここでは[チュートリアル](tutorial.md)を既に読み終わり PSDToolKit についての基本的な知識が既に備わっている方々へ向けて、ツール連携方法について紹介していきます。

# ざっくりフローチャート

あなたのニーズに合わせて何を用意すればいいかザックリと導くフローチャートです。  
最後の項目はリンクになっており、クリックすると詳細な説明に飛べます。

<svg fill="none" stroke-linecap="square" stroke-miterlimit="10" version="1.1" viewBox="0 0 900 500" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><a transform="matrix(.871 0 0 .871 -11.4 7.94)" xlink:href="#VoiceroidUtil_.2B_AITalk_.E7.B3.BB"><path d="m775 304c0-5.22 4.23-9.45 9.45-9.45h245c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-245c-5.22 0-9.45-4.23-9.45-9.45z" fill="#d9d2e9" fill-rule="evenodd"/><path d="m775 304c0-5.22 4.23-9.45 9.45-9.45h245c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-245c-5.22 0-9.45-4.23-9.45-9.45z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><text x="787.28223" y="328.86942" fill="none" font-family="sans-serif" font-size="17.3px" letter-spacing="0px" stroke="#000000" stroke-linecap="butt" stroke-width="1px" word-spacing="0px" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:125%" xml:space="preserve"><tspan x="787.28223" y="328.86942" fill="#000000" font-family="sans-serif" font-size="17.3px" stroke="none" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">VoiceroidUtil + AITalk 系製品</tspan></text></a><g transform="matrix(.871 0 0 .871 -11.4 7.94)"><path d="m617 231c0-.001.00098-.002.002-.002l267 .002c.00061 0 .001.00024.002.00066.00042.00041.00067.00099.00067.002l-.002 56.7c0 .001-.00098.002-.002.002l-267-.002c-.001 0-.002-.001-.002-.002z" fill="#d9d2e9" fill-rule="evenodd"/><path d="m617 231c0-.001.00098-.002.002-.002l267 .002c.00061 0 .001.00024.002.00066.00042.00041.00067.00099.00067.002l-.002 56.7c0 .001-.00098.002-.002.002l-267-.002c-.001 0-.002-.001-.002-.002z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><text x="633.96759" y="265.71466" fill="none" font-family="sans-serif" font-size="17.3px" letter-spacing="0px" stroke="#000000" stroke-linecap="butt" stroke-width="1px" word-spacing="0px" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:125%" xml:space="preserve"><tspan x="633.96759" y="265.71466" fill="#000000" font-family="sans-serif" font-size="17.3px" stroke="none" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">VOICEROID などの AITalk 系</tspan></text></g><a transform="matrix(.871 0 0 .871 -11.4 7.94)" xlink:href="#.E3.81.8B.E3.82.93.E3.81.97.E3.81.8F.E3.82.93_.2B_.E6.A3.92.E8.AA.AD.E3.81.BF.E3.81.A1.E3.82.83.E3.82.93_.2B_Plugin_Bouyomichan_wav_and_txt.dll"><path d="m425 508c0-5.22 4.23-9.45 9.45-9.45h375c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-375c-5.22 0-9.45-4.23-9.45-9.45z" fill="#f4cccc" fill-rule="evenodd"/><path d="m425 508c0-5.22 4.23-9.45 9.45-9.45h375c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-375c-5.22 0-9.45-4.23-9.45-9.45z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><text x="622.47723" y="520.93109" fill="#000000" font-family="sans-serif" font-size="17.3px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:125%" xml:space="preserve"><tspan x="622.47723" y="520.93109" text-align="center" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">かんしくん + 棒読みちゃん +</tspan><tspan x="622.47723" y="542.59772" text-align="center" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">Plugin_Bouyomichan_wav_and_txt.dll</tspan></text></a><a transform="matrix(.871 0 0 .871 -11.4 7.94)" xlink:href="#.E3.81.8B.E3.82.93.E3.81.97.E3.81.8F.E3.82.93_.2B_.E3.82.86.E3.81.A3.E3.81.8F.E3.82.8A.E8.AA.BF.E5.A3.B0.E3.81.8F.E3.82.93_.2B_SofTalk"><path d="m19 508c0-5.22 4.23-9.45 9.45-9.45h374c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-374c-5.22 0-9.45-4.23-9.45-9.45z" fill="#f4cccc" fill-rule="evenodd"/><path d="m19 508c0-5.22 4.23-9.45 9.45-9.45h374c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-374c-5.22 0-9.45-4.23-9.45-9.45z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><text x="46.323586" y="532.99585" fill="none" font-family="sans-serif" font-size="17.3px" letter-spacing="0px" stroke="#000000" stroke-linecap="butt" stroke-width="1px" word-spacing="0px" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:125%" xml:space="preserve"><tspan x="46.323586" y="532.99585" fill="#000000" font-family="sans-serif" font-size="17.3px" stroke="none" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">かんしくん + ゆっくり調声くん + SofTalk</tspan></text></a><a transform="matrix(.871 0 0 .871 -11.4 7.94)" xlink:href="#.E3.81.8B.E3.82.93.E3.81.97.E3.81.8F.E3.82.93_.2B_CeVIO_Creative_Studio"><path d="m50.2 353c0-5.22 4.23-9.45 9.45-9.45h320c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-320c-5.22 0-9.45-4.23-9.45-9.45z" fill="#cfe2f3" fill-rule="evenodd"/><path d="m50.2 353c0-5.22 4.23-9.45 9.45-9.45h320c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-320c-5.22 0-9.45-4.23-9.45-9.45z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><text x="67.235397" y="378.29083" fill="none" font-family="sans-serif" font-size="17.3px" letter-spacing="0px" stroke="#000000" stroke-linecap="butt" stroke-width="1px" word-spacing="0px" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:125%" xml:space="preserve"><tspan x="67.235397" y="378.29083" fill="#000000" font-family="sans-serif" font-size="17.3px" stroke="none" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">かんしくん + CeVIO Creative Studio</tspan></text></a><g transform="matrix(.871 0 0 .871 -11.4 7.94)"><path d="m360 259 115-42.9 115 42.9-115 42.9z" fill="#d9ead3" fill-rule="evenodd"/><path d="m360 259 115-42.9 115 42.9-115 42.9z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><text x="475.67441" y="253.86296" fill="#000000" font-family="sans-serif" font-size="18.7px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:125%" xml:space="preserve"><tspan x="475.67441" y="253.86296">使いたい</tspan><tspan x="475.67441" y="279.05573">ソフトは</tspan></text></g><g fill-rule="evenodd" stroke-width=".871"><path d="m503 234h22.6" fill="#000" fill-opacity="0"/><path d="m503 234h17.4" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><path d="m521 235 3.95-1.44-3.95-1.44z" fill="#000" stroke="#000" stroke-linecap="butt"/><path d="m642 259c0 15.2 10.9 30.4 21.8 30.4" fill="#000" fill-opacity="0"/><path d="m642 259c0 7.61 2.73 15.2 6.83 20.9 2.05 2.85 4.44 5.23 7 6.89.64.416 1.29.788 1.95 1.11.165.0807.33.159.495.233.0829.0374.166.0739.249.11l.211.0889" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><path d="m659 289 4.17-.597-3.57-2.22z" fill="#000" stroke="#000" stroke-linecap="butt"/><path d="m642 259c0 31.7-28.1 49.1-54 63.4-25.9 14.3-49.7 25.6-54 44.1-4.29 18.5 10.9 44.1 21.8 44.1" fill="#000" fill-opacity="0"/><path d="m642 259c0 31.7-28 49.1-54 63.4-25.9 14.3-49.7 25.6-54 44.1-2.15 9.23.576 20.2 5.19 29 2.31 4.36 5.09 8.13 7.98 10.8.722.673 1.45 1.28 2.18 1.81.364.264.729.509 1.09.735.091.0563.182.111.273.165l.0609.0352" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><path d="m551 410 4.2-.201-3.35-2.55z" fill="#000" stroke="#000" stroke-linecap="butt"/><path d="m302 234h-22.6" fill="#000" fill-opacity="0"/><path d="m302 234h-17.4" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><path d="m285 233-3.95 1.44 3.95 1.44z" fill="#000" stroke="#000" stroke-linecap="butt"/><path d="m226 259c0 12.1-11.6 18.2-23.2 24.3-11.6 6.07-23.2 12.1-23.2 24.3" fill="#000" fill-opacity="0"/><path d="m226 259c0 12.1-11.6 18.2-23.2 24.3-5.81 3.03-11.6 6.07-16 9.86-2.18 1.9-3.99 3.98-5.26 6.35-.318.593-.601 1.2-.848 1.83-.123.315-.237.634-.342.959l-.0307.0977" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><path d="m179 302 .836 4.12 2.01-3.7z" fill="#000" stroke="#000" stroke-linecap="butt"/><path d="m403 271v21.3" fill="#000" fill-opacity="0"/><path d="m403 271v16.1" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><path d="m401 288 1.44 3.95 1.44-3.95z" fill="#000" stroke="#000" stroke-linecap="butt"/><path d="m403 342c0 25-56.6 37.5-113 50-56.6 12.5-113 25-113 50" fill="#000" fill-opacity="0"/><path d="m403 342c0 25-56.6 37.5-113 50-28.3 6.25-56.6 12.5-77.9 20.3-10.6 3.91-19.5 8.21-25.7 13.1-3.1 2.44-5.53 5.03-7.19 7.79-.415.69-.781 1.39-1.1 2.1-.158.356-.304.715-.436 1.08-.0332.0903-.0656.181-.0972.272l-.0705.208" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><path d="m176 437 .784 4.13 2.06-3.67z" fill="#000" stroke="#000" stroke-linecap="butt"/><path d="m400 136c-15 0-28.2 13.4-30 24.4-1.79 11 7.87 19.6 16.6 24.4 8.76 4.82 16.6 5.86 16.6 11.7" fill="#000" fill-opacity="0"/><path d="m400 136c-15 0-28.2 13.4-30 24.4-1.79 11 7.87 19.6 16.6 24.4 4.38 2.41 8.54 3.87 11.6 5.47.765.399 1.46.806 2.08 1.24.154.108.302.218.446.329l.0743.0594" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><path d="m399 193 3.11 2.83-.565-4.17z" fill="#000" stroke="#000" stroke-linecap="butt"/><path d="m515 62.9v32.1" fill="#000" fill-opacity="0"/><path d="m515 62.9v26.9" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/></g><g fill="#000" fill-rule="evenodd" stroke-width=".871"><path d="m513 89.8 1.44 3.95 1.44-3.95z" stroke="#000" stroke-linecap="butt"/><g fill-opacity="0"><path d="m320 143h68.4v39.2h-68.4z"/><path d="m601 93.1h68.4v39.2h-68.4z"/><path d="m629 136c18.5 0 27.7-5.8 37-11.6 9.25-5.8 18.5-11.6 37-11.6"/></g></g><g fill-rule="evenodd" stroke-width=".871"><path d="m629 136c18.5 0 27.7-5.8 37-11.6 4.62-2.9 9.25-5.8 15-7.98 2.89-1.09 6.07-2 9.68-2.63 1.81-.317 3.72-.567 5.76-.737.255-.0212.512-.0413.771-.0601l.524-.035" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><path d="m698 114 3.91-1.56-4-1.31z" fill="#000" stroke="#000" stroke-linecap="butt"/><path d="m403 342c0 25 32 37.5 64 50 32 12.5 64 25 64 50" fill="#000" fill-opacity="0"/><path d="m403 342c0 25 32 37.5 64 50 16 6.25 32 12.5 44 20.3 6 3.91 11 8.21 14.5 13.1 1.75 2.44 3.13 5.03 4.07 7.79.235.69.442 1.39.621 2.1.0894.356.172.715.247 1.08l.0857.434" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><path d="m529 437 1.8 3.8 1.07-4.07z" fill="#000" stroke="#000" stroke-linecap="butt"/><path d="m642 259c0 45.5 10.9 91 21.8 91" fill="#000" fill-opacity="0"/><path d="m642 259c0 22.8 2.73 45.5 6.83 62.6 2.05 8.53 4.44 15.6 7 20.6.64 1.24 1.29 2.36 1.95 3.32.329.483.661.931.994 1.34.166.205.333.4.501.586l.0167.0181" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><path d="m659 348 4.15.677-2.75-3.19z" fill="#000" stroke="#000" stroke-linecap="butt"/></g><g transform="matrix(.871 0 0 .871 -11.4 7.94)"><path d="m472 15.8c0-5.22 4.23-9.45 9.45-9.45h245c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-245c-5.22 0-9.45-4.23-9.45-9.45z" fill="#4a86e8" fill-rule="evenodd"/><path d="m472 15.8c0-5.22 4.23-9.45 9.45-9.45h245c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-245c-5.22 0-9.45-4.23-9.45-9.45z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><text x="556.47797" y="43.423473" fill="#ffffff" font-family="sans-serif" font-size="24px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" word-spacing="0px" style="line-height:125%" xml:space="preserve"><tspan x="556.47797" y="43.423473">スタート</tspan></text></g><a transform="matrix(.871 0 0 .871 -11.4 7.94)" xlink:href="#.E5.A5.BD.E3.81.8D.E3.81.AA.E3.82.82.E3.81.AE.E4.BD.BF.E3.81.A3.E3.81.A6.E3.81.AD"><path d="m930 94.6 37.7-53.1-3.72 48.8 42.8-7.95-17 26.2 44.5 7.53-35.3 21.4 40.4 25.8-48.2-3.19 13.2 47.2-41.8-33.3-7.83 48.4-27.5-44-20.7 61.1-7.79-54.7-29.9 18.2 9.16-33.7-56 5.96 36.3-25.8-37.7-29 46.8-9.14-43.1-48.8 70.3 36.9 10.5-36.9z" fill="#f00" fill-rule="evenodd"/><path d="m930 94.6 37.7-53.1-3.72 48.8 42.8-7.95-17 26.2 44.5 7.53-35.3 21.4 40.4 25.8-48.2-3.19 13.2 47.2-41.8-33.3-7.83 48.4-27.5-44-20.7 61.1-7.79-54.7-29.9 18.2 9.16-33.7-56 5.96 36.3-25.8-37.7-29 46.8-9.14-43.1-48.8 70.3 36.9 10.5-36.9z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><text transform="translate(10.9)" fill="#ffffff" font-family="sans-serif" font-size="21.3px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" word-spacing="0px" style="line-height:100%;shape-inside:url(#rect3014);white-space:pre" xml:space="preserve"><tspan x="867.36523" y="130.944"><tspan style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">好</tspan><tspan style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">きなもの</tspan><tspan style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">
</tspan></tspan><tspan x="867.36523" y="157.069"><tspan style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">使</tspan><tspan style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">ってね！</tspan></tspan></text></a><g transform="matrix(.871 0 0 .871 -11.4 7.94)"><path d="m472 147 132-47.1 132 47.1-132 47.1z" fill="#d9ead3" fill-rule="evenodd"/><path d="m472 147 132-47.1 132 47.1-132 47.1z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><text x="604.01056" y="141.26231" fill="#000000" font-family="sans-serif" font-size="18.7px" letter-spacing="0px" stroke-linecap="butt" stroke-width="1px" text-align="center" text-anchor="middle" word-spacing="0px" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:125%" xml:space="preserve"><tspan x="604.01056" y="141.26231">音声を自動で</tspan><tspan x="604.01056" y="166.45506">挿入したい</tspan></text></g><text x="327.35196" y="150.7905" fill="none" font-family="sans-serif" font-size="20.9px" letter-spacing="0px" stroke="#000000" stroke-linecap="butt" stroke-width=".871px" word-spacing="0px" style="line-height:125%" xml:space="preserve"><tspan x="327.35196" y="150.7905" fill="#000000" font-family="sans-serif" font-size="16.3px" stroke="none" stroke-width=".871px" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">はい</tspan></text><text x="611.34857" y="116.56364" fill="none" font-family="sans-serif" font-size="20.9px" letter-spacing="0px" stroke="#000000" stroke-linecap="butt" stroke-width=".871px" word-spacing="0px" style="line-height:125%" xml:space="preserve"><tspan x="611.34857" y="116.56364" fill="#000000" font-family="sans-serif" font-size="16.3px" stroke="none" stroke-width=".871px" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">いいえ</tspan></text><g transform="matrix(.871 0 0 .871 -11.4 7.94)"><path d="m212 231c0-.00056.00046-.001.001-.001l123 .001c.00027 0 .00052.00011 7e-4.00031.00021.00019 3e-4.00045 3e-4.00071l-.001 56.7c0 .00055-.00046.001-.001.001l-123-.001c-.00056 0-.001-.00046-.001-.001z" fill="#cfe2f3" fill-rule="evenodd"/><path d="m212 231c0-.00056.00046-.001.001-.001l123 .001c.00027 0 .00052.00011 7e-4.00031.00021.00019 3e-4.00045 3e-4.00071l-.001 56.7c0 .00055-.00046.001-.001.001l-123-.001c-.00056 0-.001-.00046-.001-.001z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><text x="244.20137" y="266.24637" fill="none" font-family="sans-serif" font-size="24px" letter-spacing="0px" stroke="#000000" stroke-linecap="butt" stroke-width="1px" word-spacing="0px" style="line-height:125%" xml:space="preserve"><tspan x="244.20137" y="266.24637" fill="#000000" font-family="sans-serif" font-size="18.7px" stroke="none" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">CeVIO</tspan></text></g><g transform="matrix(.871 0 0 .871 -11.4 7.94)"><path d="m414 327c0-.00058.00046-.001.001-.001l123 .001c3e-4 0 .00055 9e-5.00073.00031.00018.00018.00031.00042.00031 7e-4l-.001 56.7c0 .00058-.00043.001-.00098.001l-123-.001c-.00055 0-.001-.00046-.001-.001z" fill="#f4cccc" fill-rule="evenodd"/><path d="m414 327c0-.00058.00046-.001.001-.001l123 .001c3e-4 0 .00055 9e-5.00073.00031.00018.00018.00031.00042.00031 7e-4l-.001 56.7c0 .00058-.00043.001-.00098.001l-123-.001c-.00055 0-.001-.00046-.001-.001z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><text x="439.76511" y="361.98758" fill="none" font-family="sans-serif" font-size="24px" letter-spacing="0px" stroke="#000000" stroke-linecap="butt" stroke-width="1px" word-spacing="0px" style="line-height:125%" xml:space="preserve"><tspan x="439.76511" y="361.98758" fill="#000000" font-family="sans-serif" font-size="18.7px" stroke="none" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">ゆっくり</tspan></text></g><a transform="matrix(.871 0 0 .871 -11.4 7.94)" xlink:href="#.E3.81.8B.E3.82.93.E3.81.97.E3.81.8F.E3.82.93_.2B_AITalk_.E7.B3.BB"><path d="m775 373c0-5.22 4.23-9.45 9.45-9.45h245c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-245c-5.22 0-9.45-4.23-9.45-9.45z" fill="#d9d2e9" fill-rule="evenodd"/><path d="m775 373c0-5.22 4.23-9.45 9.45-9.45h245c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-245c-5.22 0-9.45-4.23-9.45-9.45z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><text x="797.64288" y="398.39566" fill="none" font-family="sans-serif" font-size="17.3px" letter-spacing="0px" stroke="#000000" stroke-linecap="butt" stroke-width="1px" word-spacing="0px" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:125%" xml:space="preserve"><tspan x="797.64288" y="398.39566" fill="#000000" font-family="sans-serif" font-size="17.3px" stroke="none" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">かんしくん + AITalk 系製品</tspan></text></a><a transform="matrix(.871 0 0 .871 -11.4 7.94)" xlink:href="#.E3.81.8B.E3.82.93.E3.81.97.E3.81.8F.E3.82.93_.2B_VoiceroidUtil_.2B_AITalk_.E7.B3.BB"><path d="m651 443c0-5.22 4.23-9.45 9.45-9.45h369c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-369c-5.22 0-9.45-4.23-9.45-9.45z" fill="#d9d2e9" fill-rule="evenodd"/><path d="m651 443c0-5.22 4.23-9.45 9.45-9.45h369c2.51 0 4.91.996 6.69 2.77 1.77 1.77 2.77 4.18 2.77 6.69v37.8c0 5.22-4.23 9.45-9.45 9.45h-369c-5.22 0-9.45-4.23-9.45-9.45z" fill-rule="evenodd" stroke="#000" stroke-linecap="butt" stroke-linejoin="round"/><text x="668.69275" y="467.92322" fill="none" font-family="sans-serif" font-size="17.3px" letter-spacing="0px" stroke="#000000" stroke-linecap="butt" stroke-width="1px" word-spacing="0px" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal;line-height:125%" xml:space="preserve"><tspan x="668.69275" y="467.92322" fill="#000000" font-family="sans-serif" font-size="17.3px" stroke="none" style="font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal">かんしくん + VoiceroidUtil + AITalk 系製品</tspan></text></a></svg>

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

## かんしくん + AITalk 系

AITalk 系の製品と[かんしくん](#かんしくん)を組み合わせて使うこともできます。  
色々な音声合成ソフトを組み合わせて使う場合にはこの構成が選ばれることもあります。

### かんしくんの設定例

かんしくんの基本的な部分については[こちら](forcepser.md)を参照ください。  

VOICEROID2 や ガイノイドTalk のように複数のキャラクターが扱えるタイプのものと、VOICEROID+ 東北きりたん EX のように単体のキャラクターのみ扱えるタイプのものの、

#### 例: VOICEROID2

連動起動を利用することで、名前をつけて保存ダイアログを自動処理します。  
これはセリフを入力する際にボイスプリセットタグをつけて `東北きりたん(v1)＞こんにちは` のようにして音声保存することで反応するルールです。  
音声ファイルは AviUtl のプロジェクトファイルと同じ場所に `201231_235959_きりたん_こんにちは.wav` のような名前で配置されます。

```toml
# 注意: このファイルの文字コードは必ず UTF-8 にしてください
padding = 200
filemove = 'move'
deletetext = true

[[asas]]
exe = 'C:\Program Files (x86)\AHS\VOICEROID2\VoiceroidEditor.exe'
format = 'ボイロ2_*.wav'

[[rule]]
encoding = 'sjis'
file = 'ボイロ2_*.wav'
text = '''^東北きりたん\(v1\)＞'''
layer = 4
modifier = '''
  text = re.gsub(text, "^.*?＞", "") -- ボイスプリセットタグを除去
  text = re.gsub(text, "＜＜(.*?)｜.*?＞＞", "${1}") -- ルビを除去
  filename = os.date("%y%m%d_%H%M%S") .. "_きりたん_" .. tofilename(text, 10) .. ".wav"
'''
```

#### 例: VOICEROID+ 東北きりたん EX

連動起動を利用することで、名前をつけて保存ダイアログを自動処理します。  
音声ファイルは AviUtl のプロジェクトファイルと同じ場所に `201231_235959_きりたん_こんにちは.wav` のような名前で配置されます。

```toml
# 注意: このファイルの文字コードは必ず UTF-8 にしてください
padding = 200
filemove = 'move'
deletetext = true

[[asas]]
exe = 'C:\Program Files (x86)\AHS\VOICEROID+\KiritanEX\VOICEROID.exe'
format = 'きりたん_*.wav'

[[rule]]
encoding = 'sjis'
file = 'きりたん_*.wav'
layer = 1
modifier = '''
  text = re.gsub(text, "＜＜(.*?)｜.*?＞＞", "${1}") -- ルビを除去
  filename = os.date("%y%m%d_%H%M%S") .. "_きりたん_" .. tofilename(text, 10) .. ".wav"
'''
```

## かんしくん + VoiceroidUtil + AITalk 系

[VoiceroidUtil のみを使って自動挿入する](#VoiceroidUtil_+_AITalk_系)こともできますが、[かんしくん](#かんしくん)を組み合わせて使うこともできます。  
色々な音声合成ソフトを組み合わせて使う場合にはこの構成が選ばれることもあります。

この構成で使用するなら VoiceroidUtil はデフォルト設定のまま使うことができます。

### かんしくんの設定例

かんしくんの基本的な部分については[こちら](forcepser.md)を参照ください。  
VoiceroidUtil がすべてデフォルト設定なら以下のような設定で動作します。

VoiceroidUtil を通して音声ファイルを作成すると、AviUtl のプロジェクトファイルと同じフォルダーへコピーしてから投げ込みます。  
`filemove = 'move'` だと VoiceroidUtil の動作に干渉してしまうので注意してください。

```txt
# 注意: このファイルの文字コードは必ず UTF-8 にしてください
padding = 200
filemove = 'copy'
deletetext = true

[[asas]]
exe = 'C:\Users\YourName\Documents\VoiceroidUtil\VoiceroidUtil.exe'
flags = 0 # 「名前を付けて保存」ダイアログの自動処理は無効

[[rule]]
dir = 'C:\Users\YourName\Documents\VoiceroidWaveFiles'
file = '*_東北きりたん(v1)_*.wav'
encoding = 'sjis'
layer = 1
```

## かんしくん + 棒読みちゃん + Plugin_Bouyomichan_wav_and_txt.dll

[棒読みちゃん](https://chi.usamimi.info/Program/Application/BouyomiChan/) に [Plugin_Bouyomichan_wav_and_txt.dll](https://github.com/yukimaru73/Plugin_Bouyomichan_wav_and_txt) を導入すると、音声ファイルとテキストファイルを同時に保存できるようになります。

### プラグインを導入済みの棒読みちゃんの初回起動から連携設定までの最短ルート

1. 棒読みちゃんを起動します。
2. `音声 テキスト出力` プラグインを有効にします。  
（※それ以外のプラグインは連携に必要ありません）
3. `その他` タブをクリックします。
4. `音声 テキスト出力` を選択し、下にあるスパナアイコンをクリックします。
5. ファイルの保存場所をかんしくんと同じ場所にある `tmp` フォルダーに設定します。

これで `音声合成` タブに戻り、セリフを入力してツールバー右上のほうにある `🛑/📝` のようなボタンを押すと音声とテキストの保存ができます。

### かんしくんの設定例

かんしくんの基本的な部分については[こちら](forcepser.md)を参照ください。  
`201231_235959_女性１_V@100_S@100_T@100_こんにちは.wav` のような名前でファイルが作成されると反応するルールです。

```txt
# 注意: このファイルの文字コードは必ず UTF-8 にしてください
padding = 200
filemove = 'move'
deletetext = true

[[asas]]
exe = 'C:\Users\YourName\Documents\BouyomiChan\BouyomiChan.exe'
flags = 0 # 「名前を付けて保存」ダイアログの自動処理は無効

[[rule]]
encoding = 'sjis'
file = '*_女性１_*.wav'
layer = 1
modifier = '''
  filename = os.date("%y%m%d_%H%M%S") .. "_女性１_" .. tofilename(text, 10) .. ".wav"
'''
```

## かんしくん + ゆっくり調声くん + SofTalk

[SofTalk](https://w.atwiki.jp/softalk/pages/1.html) はそれ単体でも音声とテキストファイルを保存することができますが、[ゆっくり調声くん](https://www.nicovideo.jp/watch/sm32835672)と組み合わせると詳細な調声やセリフの再編集なども行いやすくなります。

※ SofTalk ver1.93.60 以降は AquesTalk への対応が中止されているため、この組み合わせでいわゆる「ゆっくり音声」を使用することはできなくなりました。

### ゆっくり調声くん + SofTalk の初回起動から連携設定までの最短ルート

1. ゆっくり調声くんを起動します。
2. `設定` タブをクリックします。
3. 一番下にある `SofTalk.exe ファイルパス` に SofTalk.exe のファイルパスを指定します。
4. `音声作成` タブをクリックします。
5. `ファイル` メニューから `新規プロジェクト開始` を選びます。
6. プロジェクトのフォルダーを選択します。  
（例えば `C:\Users\YourName\ゆっくり調声くんプロジェクト` など）

これで左上の `新規作成` を選び、緑の枠の一番上の入力欄にセリフを入力して `変換` ボタンを押すと読み上げに関する設定画面が現れます。  
この状態で下の `挿入` ボタン押すと上のセリフ一覧にセリフが追加され、それと同時に音声とテキストが保存されます。

### かんしくんの設定例

かんしくんの基本的な部分については[こちら](forcepser.md)を参照ください。  
ゆっくり調声くんのプロジェクトを `C:\Users\YourName\ゆっくり調声くんプロジェクト` に配置している場合、以下のような設定で動作します。

```txt
# 注意: このファイルの文字コードは必ず UTF-8 にしてください
padding = 200
filemove = 'move'
deletetext = true

[[asas]]
exe = 'C:\Users\YourName\Documents\YukkuriChoseiKun\YukkuriChoseiKun.exe'
flags = 0

[[rule]]
encoding = 'sjis'
dir = 'C:\Users\YourName\ゆっくり調声くんプロジェクト'
file = '*_霊夢_*.wav'
layer = 1
modifier = '''
  filename = os.date("%y%m%d_%H%M%S") .. "_霊夢_" .. tofilename(text, 10) .. ".wav"
'''
```

## かんしくん + CeVIO Creative Studio

[CeVIO Creative Studio](http://cevio.jp/) では保存時にテキストも同時に保存することができます。

CeVIO Creative Studio での事前の設定は特に必要なく、トークトラックに適当にセリフを入力し、`Ctrl + W` を押すか、右クリックメニューから `WAV書き出し(W)...` を選び、出てきた保存ダイアログの下にある `セリフをテキスト出力` にチェックを付けた状態で音声ファイルを保存することで、音声とテキストが同時に保存されます。

かんしくんでの連動起動を使う前に、一度上記の手順で音声とテキストの両方を作成しておいてください。  
連動起動すると「名前を付けて保存」ダイアログが自動処理されるため、設定を変更することができなくなります。

### かんしくんの設定例

かんしくんの基本的な部分については[こちら](forcepser.md)を参照ください。

```txt
# 注意: このファイルの文字コードは必ず UTF-8 にしてください
padding = 200
filemove = 'move'
deletetext = true

[[asas]]
exe = 'C:\Program Files\CeVIO\CeVIO Creative Studio (64bit)\CeVIO Creative Studio.exe'

[[rule]]
encoding = 'sjis'
file = '*_さとうささら_*.wav'
layer = 1
modifier = '''
  text = re.gsub(text, "｜?([^｜《》]*)《.*?》", "${1}") -- ルビを除去
  filename = os.date("%y%m%d_%H%M%S") .. "_ささら_" .. tofilename(text, 10) .. ".wav"
'''
```
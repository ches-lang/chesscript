# ChesScript - JavaScript transpiler for Ches

Ches → JavaScript のトランスパイラです。コンパイル先の JavaScript バージョンを指定することも可能です。

## サンプルコード

> Ches (Prototype)

```
pub fn main()
    println("Hello, world!")
end
```

> JavaScript (ES2015)

```
export function main() {
    console.log('Hello, world!');
}
```

※ 初期の Ches ランタイム環境はプロトタイプverとして開発されます。

※ 仕様が不安定なので仕様変更の可能性があります。

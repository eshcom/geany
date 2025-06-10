s.match(/yes.*day/s);

const re = new RegExp(/ab+c/, "i"); // constructor with regular expression literal as first argument
const re = new RegExp("\\w+");
const re = /\d/y;
const re = /[\u0400-\u04ff]+/g;

const url = "http://xxx.domain.com";
console.log(/^https?:\/\/(.+?)\./.exec(url)[1]); // 'xxx'

order.match(new RegExp(`\\b(${breakfasts.join("|")})\\b`, "g"));

"x".replace(/x(.)?/g, (m, group) => {
  console.log(`group: ${JSON.stringify(group)}`);
});

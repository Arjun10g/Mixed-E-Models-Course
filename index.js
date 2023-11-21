function clampBuilder( minWidthPx, maxWidthPx, minFontSize, maxFontSize ) {
    const root = document.querySelector( "html" );
    const pixelsPerRem = Number( getComputedStyle( root ).fontSize.slice( 0,-2 ) );
  
    const minWidth = minWidthPx / pixelsPerRem;
    const maxWidth = maxWidthPx / pixelsPerRem;
  
    const slope = ( maxFontSize - minFontSize ) / ( maxWidth - minWidth );
    const yAxisIntersection = -minWidth * slope + minFontSize
  
    return `clamp( ${ minFontSize }rem, ${ yAxisIntersection }rem + ${ slope * 100 }vw, ${ maxFontSize }rem )`;
  }
  


  console.log( clampBuilder( 320, 1280, 0.75, 1.4 ) );

let open = document.querySelector('.open');
let close = document.querySelector('.close');
let nav = document.querySelector('.nav');
let navEls = document.querySelectorAll('.nav > *');
navEls = Array.from(navEls);

const evenElements = navEls.filter((element, index) => index % 2 === 0);
const oddElements = navEls.filter((element, index) => index % 2 !== 0);

// Now, evenElements will contain even-indexed elements, and oddElements will contain odd-indexed elements
console.log("Even Elements:", evenElements);
console.log("Odd Elements:", oddElements);


open.addEventListener('click', function(){
    let tl1 = gsap.timeline();
    tl1.to('.open', {duration: 0.5, opacity: 0})
    .set('.close', {display: 'block'}, '-=50%')
    .to('.close', {duration: 0.5, opacity: 1}, '<')
    .set('.open', {display: 'none', }, "<")
    .set('.nav', {display: 'flex'})
    .fromTo('.nav', {opacity: 0}, {duration: 1, opacity: 1})
    .fromTo(evenElements, {x:"-100vw"}, {duration: 2, x:0})
    .fromTo(oddElements, {x:"100vw"}, {duration: 2, x:0}, "<")
});

close.addEventListener('click', function(){
    let tl2 = gsap.timeline();
    tl2.to('.close', {duration: 0.5, opacity: 0})
    .set('.open', {display: 'block'}, '-=50%')
    .to('.open', {duration: 0.5, opacity: 1}, '<')
    .set('.close', {display: 'none'})
    .fromTo(evenElements, {x:0}, {duration: 2, x:"-100vw"})
    .fromTo(oddElements, {x:0}, {duration: 2, x:"100vw"}, "<")
    .to('.nav', {duration: 1, opacity: 0}, "<")
    .set('.nav', {display: 'none'})
});


// Code Fixer

let code = document.querySelectorAll('.code');
let codeHead = document.querySelectorAll('.code-head button');
code.forEach((block) => {
    block.textContent = block.textContent.replace(/(?<=\n)\s+/g, '').replace(/\nbr/g, '\n').replace(/%>% \n/g, '%>%\n\t').replace(/\+\n/g, '+\n\t').replace(/^\s+/, '').replace(/\\t/g, '\t');
});

let rCode = document.querySelectorAll('.rcode');
rCode.forEach((block) => {
    block.textContent = block.textContent.replace(/(?<=\n)\s+/g, '').replace(/\nbr/g, '\n').replace(/%>% \n/g, '%>%\n\t').replace(/\+\n/g, '+\n\t').replace(/^\s+/, '').replace(/\\t/g, '\t');
});


codeHead.forEach((block, index) => {
    block.addEventListener('click', function(){
        block.classList.toggle('active-code-head');
        if(block.classList.contains('active-code-head')){
            block.textContent = 'Hide';
            code[index].classList.add('active-code');
            gsap.from(code[index], {duration: 1, opacity: 0, y: 50})
        } else{
            block.textContent = 'Show';
            code[index].classList.remove('active-code');
        }
    });
});

// Find h2 contents 

// let h2 = document.querySelectorAll('.tb-meth h2');
// let h2Content = [];
// h2.forEach((block) => {
//     h2Content.push(block.textContent);
// });

// // For each h2 assign a unique id

// h2.forEach((block, index) => {
//     block.setAttribute('id', `tb-meth${index}`);
// }
// );


let btn = document.querySelector('.play');
let frame = document.querySelector('.frame');

btn.addEventListener('click', () => {
    frame.classList.toggle('inactive');
    gsap.from(frame, { opacity: 0, xPercent:-100 ,duration: 1 });
});


let listItems = document.querySelectorAll('.nav > p');
let contentItems = document.querySelectorAll('div[class ^= "content"]')

listItems = Array.from(listItems);
contentItems = Array.from(contentItems);

listItems.forEach((item, index) => {
  item.addEventListener('click', () => {
    contentItems.forEach((content, contentIndex) => {
      if (contentIndex === index) {
        content.style.display = 'block';
      } else {
        content.style.display = 'none';
      }
    });
  });
});
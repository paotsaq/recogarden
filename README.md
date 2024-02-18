<h1>recogarden: automating Tiddler creation from a private API</h1> 

There's, err, not much context I can give, but this was done in [Chicken Scheme](https://wiki.call-cc.org/) and I'm proud of my first project in a ~~proper~~ functional programming paradigm!

## Ok, Alex — what's is this all about now?

Hello! [^1] 

For a long time I have been invested in a music-related private API. It has a database akin to [Discogs](https://discogs.com) or [Rate Your Music](https://rateyourmusic.com) — both also great candidates for some automation work —, and I wanted to create tiddlers out of its content. 

Look — I made a drawing! 🎨

![https://sbsbsb.sbs/images/ambient1-resized.jpg](https://sbsbsb.sbs/images/recogarden_diagram.png)

### That looks nice! But why Scheme? 💡

For this new project I was set on [feeding two birds with one scone](https://www.peta.org/teachkind/lesson-plans-activities/animal-friendly-idioms/): not only solve something that I wanted solved in the first place, but also to do it in a language (and paradigm) in which I am not so comfortable with.

Oh, and ideally I'd use some of the good practices from [How To Design Programs](https://sbsbsb.sbs/How%2520To%2520Design%2520Programs). So this is a very simple script that 

a) does some API calls;

b) parses the results;

c) writes some files.

Once again, not something I'd expect to be useful for anyone else in the world...

### ...and yet you're quite happy about it, aren't you?

Oh yes. Indeed I am! ✨

Working in Python is rather easy by now. Whenever I struggle, it's *rare* that the source of problems is the language itself. Scheme, on the other hand, has a whole different syntax — descending from the [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)) family of languages — and I had to wrap my brain around some new ideas and ways of handling things.

Thus, in this case, it was a reversal of the usual scenario: to solve a *simple* problem in a language I was (although now a little less!) uncomfortable with. 

### That seems great, Alex. I'd say you did a good job, but people would notice you'd be congratulating *yourself*, as I am no more than a role-play you imagined for the sake of entertaining your audience. By the way, is this finished?

Of course not. But I am moving on, and will iterate on the code as I get more comfortable with the language.

I see some possible applications of threading as an exercise, and I did not explore pipelines that would make code compilation & execution more comfortable. 

This project was fundamentally an exercise in *quick feedback loops*, which is something I care about more and more, and effective problem isolation: it took a bit of time to get familiar with the compiler errors, and encapsulating problems in lesser code was, too, very productive. 

#### You know, that reminds me: in interviews to very coveted professional positions, applicants are often treated to the delight of recounting their serendipitous *aha!* moments in programming, interwoven with a rich and intrinsically personal journey of transcending space and time...

...right...?

#### ... all the while showcasing their resilience, and innate genius in solving the toughest coding conundrums... 

...err...

#### ...because, as you know, we're all just casually ripping holes in the fabric of reality to commune with the sacred truths of computer science. 

\[🤔 intensifies\]

### So I wonder, Alex, do you have any strong feelings to share after you went through this project?

I mean — nothing too fancy was happening, really; *there really are lots of parenthesis*, and many variants of `let` (I count at least five). Careless coding might provoke disastrous arrangements of parens.

In summary: `x` is a value, `(x)` is a call on `x` (which is often not what one wants) and `(f x)` is `x` passed as an argument to `f`. Since everything is wrapped in parenthesis, this very simple syntax might lead to confusing situations. This is not really relevant, but you just learned, say, 50% of Scheme just there. You're welcome!

Below is a short example of two different `let` formulations (they're useful to define local scope variables). I will not disclose how long it usually took me to untangle myself of errors of this nature, but my impression is that it takes a while to get the hang of this syntax.


```scheme
(let ((x 2)
      (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (* z x))) 

(let*-values (((a b) (values 2 3))
              ((p) (+ a b)) )
```

### That's cool, even if slighty underwhelming. 

Wait — let me rephrase it. <sup>Ahem</sup>


> In my Scheme endeavor, I whimsically toyed with monadic musings in the spirit of Haskell, playfully juggling polymorphic prestidigitation and functorial flummery to whimsically shuffle lists with the scientific precision of a quantum entangled particle performing a multidimensional dance.

### Ah, that's better. 

I'm so glad you enjoyed it <sup>but it was ChatGPT, though.</sup>

### Oh, I don't mind — it really scratched my itch of projecting worlds I do not necessarily comprehend but would love to contemplate from a distance. 🪐 

You do you! Well — off I go to code some other stuff! 🤸


[^1]: — [again](https://github.com/paotsaq/kobogarden/)?

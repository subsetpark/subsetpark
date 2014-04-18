Title: Abba
Date: 2014-03-19
<!-- Status: draft -->

My first major project at [Hacker School][hs] is called [Abba](http://github.com/subsetpark/abba), the Abbreviation Engine. In order to understand why Abba exists I need to backtrack a little.

## The New Abbreviations

Since around 2006 I have written in an alphabetic shorthand that I call (when I have to write about it in places like this) [the New Abbreviations][tna]. When asked for a snappy one-line I say it's basically a 'human-readable non-lossy compression algorithm for text'. It's also of the same general family as the [*abbreviationes*][abbreviations] of the Middle Ages. It looks like this:

![TNA_sample](http://i.imgur.com/048po.jpg)

[hs]: http://hackerschool.com
[tna]: https://thoughtstreams.io/zdsmith/new-abbreviations/
[abbreviations]: http://en.wikipedia.org/wiki/Scribal_abbreviation

Very simply, the New Abbreviations (and alphabetic shorthands in general) are a matter of matching some simple lettern patterns and replacing them with different symbols. 

I've written this way for a while. It's a long-standing project of mine of which I'm rather fond. And on the face of it, this should be very simple to digitize. 

There are just a couple factors which make it non-trivial to implement as a simple set of find-and-replace rules:

1. Positioning rules. This shorthand was designed with English text in mind, and so there are certain assumptions about character placement built in. For instance: one very common word that's abbreviated in this shorthand is the word "we", which comes out as *w̃*. That's pretty readable, especially when you combine it with its fellows, *m̃* for "me" and *ũ* for "us". But it wouldn't be as useful to read and write if you simply replaced the sequence "we" every time those two letters came together, leaving *sw̃ll* and *sw̃ep* and others. So you replace "we" the word only when it stands alone. And similarly, you replace the prefix "con" and the suffix "ion". So you need to translate human understandings of morphology, to an extent, into abbreviation rules.

2. Non-Unicode glyphs. Above we saw that "we" gets abbreviated as *w̃*, which is all well and good because we can type out *w̃* with relatively little difficulty. But shorthands are not typed, they're written; and thus they'll tend to contain symbols that you can't necessarily represent in computer text—and certainly not with perfect semantic fidelity. In my shorthand, for instance, "er", when found in the middle of a word, is rendered with a little curlicue above the x-height, as seen in the wikipedia entry for [Scribal Abbrevations][abbreviations] under "forms". There's no unicode character for that that I've been able to find. So you can't simply replace several letters with one letter—sometimes what you're inserting won't be a letter at all.

## Implementation

It was with these technical hurdles in mind that I decided to come up with a Python application that could do the work of my shorthand system. 

Of course, it would hardly be worth my while to write a program that only spoke the New Abbreviations. It only made sense to write a more generic shorthand engine, which could understand the NA as a ruleset, as well as any other hypothetical alphabetical shorthand that somebody else might like to come up with. 

In any case, the first thing I knew was that you couldn't just stick funny unicode letters into a given text and call it a day. If you could I never would have written a program in the first place; I would have just assembled a huge unwieldy collection of unicode symbols and sufficed myself with a lot of copy and pasting. 

Instead, I figured: since strings and lists (of characters) are so closely related in Python, why not replace the sequences of characters you want to replace with abbreviation objects, that can report on their own realization? That way you don't have to be confined to the Unicode character set when defining your abbreviations. 

For the first day I continued in this vein, and actually came up with a working prototype. Unfortunately the code was turning out nearly unmaintainable. There was too much strain and accounting for the fact that when dealing with a heterogenous list you never knew what type of thing the next element in your list would be. So every list operation ended up having to wrap itself in checks for element types. And the regexes that determine string replacement don't make any sense when applied to non-text objects.

### The Dark Heart of Abba

The breakthrough came with the idea to *use Unicode private use characters to stand in for abbreviations* in a string. A brief primer:

#### Unicode Private Use Areas

Unicode, you may or may not know, is a text encoding standard for representing a much wider range of characters than just the Latin alphabet encoded in ASCII. The general aim of this sort of thing is to provide a standard encoding for any letter or character in any language in the world. It's organized by *blocks* of codepoints, where each codepoint is the hexadecimal number assoociated with the encoded character. The Unicode character 'a', for instance, has the hex codepoint 0x061, which is 97 in decimal. The Gurmukhi letter 'ਉ' has the hex codepoint 0xa09, which is 2569. It's the 2569th character in the Unicode character set.

The 57344th character in the Unicode charset, at codepoint 0xE000, doesn't have a character associated with it. That its whole function, actually—to not have a letter associated with it. It's a Unicode codepoint that acts like a character and can live in a string with other characters but it has no standardized content associated with it. The whole block of which it is the first character is the [Private Use Area](http://en.wikipedia.org/wiki/Private_Use_Areas).

### Using Private Use Characters as Abbreviations

So, if we use the Private Use Area of the Basic Multilingual Plane of the Unicode standard, we have 6400 codepoints that we can insert into our text with the near-certainty that they won't already be there. And then we can treat our list of chars like a real grown-up string. 

```python
def __init__(self, config):
    self.abb_sequences = []
    self.lookup_table = {}
    # begin creating unicode characters at the beginning 
    # of the private use space
    self.pool = iter(range(57344,63743))
    rep_search = re.compile("_rep$")
    # Read through the config file, pulling out abbreviation schemae
    for section in config.sections():
        has_a_rep = False
        # Pull a control character from the pool
        codepoint = chr(next(self.pool))
        # Analyze the regnet markup and move it into the abbreviation dict
        self.add_to_sequences(section, regnet.Regnet(config[section]['pattern']), codepoint)
        for option in config.options(section):
            # Go through each section's options. If it has _rep in it,
            # It's a representation method. Add it to the lookup.
            if re.search(rep_search, option):
                has_a_rep = True
                self.add_to_lookup(codepoint, section, option=option, 
                    value=regnet.parse_regnet(list(config[section][option])))
        if not has_a_rep: 
            self.add_to_lookup(codepoint, section)              
```

Here's how it works: `abba` creates a register of abbreviations that it reads in from the ruleset. For each abbreviation, it dynamically assigns one of the Unicode codepoints. First it associates that codepoint with the regexp (the regnet, really—more on that later) belonging to that abbreviation. Then it adds that codepoint to a lookup table containing that abbreviation's representations, whatever they happen to be. In both cases the codepoint acts as a stand-in for the abbreviation object, but we are able to use all the information encoded about the abbreviation without making any assumptions as to its composition.

### Regnet

The other half of abba is the Regnet format, which is a user-friendly way to define abbreviation rules. Under the hood, abba uses regular expressions to find letter sequences and replace them with codepoints. But I wanted to make abba extensible, and regular expressions are not fun to write by anybody, myself included. So I created a format called Regnet (which means 'The Rain' in Swedish and is the first Swedish word beginning with "Re-" to be found [here](http://en.wikipedia.org/wiki/Abba)). Regnet lets you take something like `the#iso` and turn it into an object that maps `(?<=\\\\b)\g<pat>(?=\\\\b)` onto *ð* with a precedence level of 0 (which means "the" will get abbreviated before, say, *terminal e*). Here's a sample of the ruleset for the New Abbreviations in Regnet format:

```
[UN]
pattern: un
uni_rep: u${lin_low}

[R ROTUNDA]
pattern: (?<=[bdhmnopquw])r
uni_rep: {a75b}

[THE]
pattern: the#iso
uni_rep: ${th}
```

### abba in Action

You put it together and you're able to abbreviate texts in an extensible and generalizable manner. Here's a sample text produced that models some (though not all) of the New Abbreviations' rules: 

> Pꝛut, tut, ſa͞d Pãagrṷl, Ⱳt doð ðis fo̥l mean to ſay? I ðḭ h̭ is upon ð foꝛgi̫ of ſom̭ diabolical to̫ṷ, ⁊ ðt enchãer-lik̭ h̭ wo̬ld charm us. To Ⱳom oṋ of his men ſa͞d, Wiðo̬t do̬bt, ſir, ðis fe‖ow wo̬ld cõ̬erf̭͞t ð la̫uag̭ of ð Pariſians, but h̭ doð only flay ð Latin, imagini̫ by ſo do̫͞ ðt h̭ doð highly Piͫariz̭ it in moſt eloq̭̇̃ terms, ⁊ ſtro̫ly conc̭͞teð himſelf to b̭ ðrfoꝛ̭ a great oꝛatoꝛ in ð French, beca̬ſ̭ h̭ diſda͞neð ð common manner of ſpeaki̫. To Ⱳich Pãagrṷl ſa͞d, Is it trṷ? ð ſcholar anſwer̳, My woꝛſhipful loꝛd, my geni̭ is not apt naṱ to ðt Ⱳich ðis flagitio̬s nebulon ſa͞ð, to excoꝛiaṱ ð cut(ic)uḽ of o̬r vernacular Ga‖ic, but vic̭-verſa‖y I gnav̭ oper̭, ⁊ by veḽs ⁊ ram̭s eniṱ to locupletaṱ it WITH ð Latinicom̭ reduͫanc̭. By G—, ſa͞d Pãagrṷl, I wi‖ teach ỿ to ſpeak. But firſt com̭ hiðer, ⁊ te‖ m̃ Ⱳenc̭ ðo̬ art. To ðis ð ſcholar anſwer̳, ð pꝛimeval oꝛigin of my av̭s ⁊ atav̭s was iͫigenary of ð Lemovic regɸs, Ⱳr r̭q̇i̭ſceð ð coꝛpoꝛ of ð hagiotat ſt. Martial. I uͫerſtaͫ ð̭̭ very we‖, ſa͞d Pãagrṷl. Ⱳn a‖ com̭s to a‖, ðo̬ art a Limo̬ſin, ⁊ ðo̬ wilt her̭ by ðy aff̭ɥ̳ ſp̭̭ch cõ̬erf̭͞t ð Pariſians. We‖ now, com̭ hiðer, I muſt ſhow ð̭̭ a new trick, ⁊ haͫſomely giv̭ ð̭̭ ð combfeat. WITH ðis h̭ to̥k him by ð ðꝛoat, ſay̫͞ to him, ðo̬ flay̭ſt ð Latin; by ſt. John, I wi‖ mak̭ ð̭̭ flay ð fox, foꝛ I wi‖ now flay ð̭̭ aliv̭. ðn began ð po̥ꝛ Limo̬ſin to cry, Haw, gwid maaſter! haw, Laoꝛd, my halp, ⁊ ſt. Marſhaw! haw, I'm woꝛri̭d. Haw, my ðꝛoppḽ, ð bean of my cragg is bꝛuck! Haw, foꝛ ga̬ad's ſeck lawt my lean, mawſter; waw, waw, waw. Now, ſa͞d Pãagrṷl, ðo̬ ſpeak̭ſt natuꝛa‖y, ⁊ ſo let him go, foꝛ ð po̥ꝛ Limo̬ſin had tota‖y bewꝛay̭d ⁊ ðoꝛo̬ghly conſhit his bꝛ̭̭ch̭s, Ⱳich wer̭ not ḓ̭p ⁊ larg̭ eno̬gh, but ro̬ͫ ſtra͞ght çnɸ̳ gregs, havi̫ in ð ſeat a pi̭c̭ lik̭ a k̭̭li̫'s ta͞l, ⁊ ðrfoꝛ̭ in French ca‖̳, ḓ cha̬ſſ̭s a q̭̬̭̇ ḓ merlus. ðn, ſa͞d Pãagrṷl, ſt. Alipãin, Ⱳt civet? Fi̭! to ð devil WITH ðis tuꝛnip-eater, as h̭ ſtḭs! ⁊ ſo let him go. But ðis hug of Pãagrṷl's was ſuch a terroꝛ to him a‖ ð days of his lif̭, ⁊ to̥k ſuch ḓ̭p impꝛ̭ſſɸ in his fancy, ðt very often, diſtraɥ̳ WITH ſudden affrightm̭̃s, h̭ wo̬ld ſtartḽ ⁊ ſay ðt Pãagrṷl held him by ð neck. B̭ſiḓs ðt, it pꝛocuꝛ̳ him a cõinual dꝛo̬ght ⁊ ḓſir̭ to dꝛḭ, ſo ðt after ſom̭ few y̭ars h̭ di̭d of ð deað Rolaͫ, in pla͞n ̭̫liſh ca‖̳ ðirſt, a woꝛk of diviṋ v̭̫eanc̭, ſhowi̫ us ðt Ⱳich ſa͞ð ð philoſopher ⁊ A̬lus Ge‖i̬s, ðt it becomeð us to ſpeak accoꝛdi̫ to ð common la̫uag̭; ⁊ ðt w̃ ſho̬ld, as ſa͞d Oɥavian A̬guſtus, ſtriv̭ to ſhu̲ a‖ ſtra̫̭ ⁊ ṵnown terms WITH as much h̭̭dfulṋſs ⁊ circumſp̭ɥɸ as pilots of ſhips uſ̭ to avo͞d ð rocks ⁊ ba̰s in ð ſeaa͞d Pãagrṷl, Ⱳt doð ðis fo̥l mean to ſay? I ðḭ h̭ is upon ð foꝛgi̫ of ſom̭ diabolical to̫ṷ, ⁊ ðt enchãer-lik̭ h̭ wo̬ld charm us. To Ⱳom oṋ of his men ſa͞d, Wiðo̬t do̬bt, ſir, ðis fe‖ow wo̬ld cõ̬erf̭͞t ð la̫uag̭ of ð Pariſians, but h̭ doð only flay ð Latin, imagini̫ by ſo do̫͞ ðt h̭ doð highly Piͫariz̭ it in moſt eloq̭̇̃ terms, ⁊ ſtro̫ly conc̭͞teð himſelf to b̭ ðrfoꝛ̭ a great oꝛatoꝛ in ð French, beca̬ſ̭ h̭ diſda͞neð ð common manner of ſpeaki̫. To Ⱳich Pãagrṷl ſa͞d, Is it trṷ? ð ſcholar anſwer̳, My woꝛſhipful loꝛd, my geni̭ is not apt naṱ to ðt Ⱳich ðis flagitio̬s nebulon ſa͞ð, to excoꝛiaṱ ð cut(ic)uḽ of o̬r vernacular Ga‖ic, but vic̭-verſa‖y I gnav̭ oper̭, ⁊ by veḽs ⁊ ram̭s eniṱ to locupletaṱ it WITH ð Latinicom̭ reduͫanc̭. By G—, ſa͞d Pãagrṷl, I wi‖ teach ỿ to ſpeak. But firſt com̭ hiðer, ⁊ te‖ m̃ Ⱳenc̭ ðo̬ art. To ðis ð ſcholar anſwer̳, ð pꝛimeval oꝛigin of my av̭s ⁊ atav̭s was iͫigenary of ð Lemovic regɸs, Ⱳr r̭q̇i̭ſceð ð coꝛpoꝛ of ð hagiotat ſt. Martial. I uͫerſtaͫ ð̭̭ very we‖, ſa͞d Pãagrṷl. Ⱳn a‖ com̭s to a‖, ðo̬ art a Limo̬ſin, ⁊ ðo̬ wilt her̭ by ðy aff̭ɥ̳ ſp̭̭ch cõ̬erf̭͞t ð Pariſians. We‖ now, com̭ hiðer, I muſt ſhow ð̭̭ a new trick, ⁊ haͫſomely giv̭ ð̭̭ ð combfeat. WITH ðis h̭ to̥k him by ð ðꝛoat, ſay̫͞ to him, ðo̬ flay̭ſt ð Latin; by ſt. John, I wi‖ mak̭ ð̭̭ flay ð fox, foꝛ I wi‖ now flay ð̭̭ aliv̭. ðn began ð po̥ꝛ Limo̬ſin to cry, Haw, gwid maaſter! haw, Laoꝛd, my halp, ⁊ ſt. Marſhaw! haw, I'm woꝛri̭d. Haw, my ðꝛoppḽ, ð bean of my cragg is bꝛuck! Haw, foꝛ ga̬ad's ſeck lawt my lean, mawſter; waw, waw, waw. Now, ſa͞d Pãagrṷl, ðo̬ ſpeak̭ſt natuꝛa‖y, ⁊ ſo let him go, foꝛ ð po̥ꝛ Limo̬ſin had tota‖y bewꝛay̭d ⁊ ðoꝛo̬ghly conſhit his bꝛ̭̭ch̭s, Ⱳich wer̭ not ḓ̭p ⁊ larg̭ eno̬gh, but ro̬ͫ ſtra͞ght çnɸ̳ gregs, havi̫ in ð ſeat a pi̭c̭ lik̭ a k̭̭li̫'s ta͞l, ⁊ ðrfoꝛ̭ in French ca‖̳, ḓ cha̬ſſ̭s a q̭̬̭̇ ḓ merlus. ðn, ſa͞d Pãagrṷl, ſt. Alipãin, Ⱳt civet? Fi̭! to ð devil WITH ðis tuꝛnip-eater, as h̭ ſtḭs! ⁊ ſo let him go. But ðis hug of Pãagrṷl's was ſuch a terroꝛ to him a‖ ð days of his lif̭, ⁊ to̥k ſuch ḓ̭p impꝛ̭ſſɸ in his fancy, ðt very often, diſtraɥ̳ WITH ſudden affrightm̭̃s, h̭ wo̬ld ſtartḽ ⁊ ſay ðt Pãagrṷl held him by ð neck. B̭ſiḓs ðt, it pꝛocuꝛ̳ him a cõinual dꝛo̬ght ⁊ ḓſir̭ to dꝛḭ, ſo ðt after ſom̭ few y̭ars h̭ di̭d of ð deað Rolaͫ, in pla͞n ̭̫liſh ca‖̳ ðirſt, a woꝛk of diviṋ v̭̫eanc̭, ſhowi̫ us ðt Ⱳich ſa͞ð ð philoſopher ⁊ A̬lus Ge‖i̬s, ðt it becomeð us to ſpeak accoꝛdi̫ to ð common la̫uag̭; ⁊ ðt w̃ ſho̬ld, as ſa͞d Oɥavian A̬guſtus, ſtriv̭ to ſhu̲ a‖ ſtra̫̭ ⁊ ṵnown terms WITH as much h̭̭dfulṋſs ⁊ circumſp̭ɥɸ as pilots of ſhips uſ̭ to avo͞d ð rocks ⁊ ba̰s in ð ſea.
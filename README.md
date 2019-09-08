[![codecov](https://codecov.io/gh/greebie/DNV-format/branch/master/graph/badge.svg)](https://codecov.io/gh/greebie/DNV-format)

# DNV PARSER USAGE - Quick Start

Assuming you have a DNV format file at "/path/file.dnv":

```
graph = Graph("/path/file.dnv")
```

Will create a graph objects.  If you want to see the nodes type:

```
graph.nodes
```

similarly, the edges:

```
graph.edges
```

As the project grows, there will be other output options for Graphml and Gexf,
for instance.

# DNV (Delimited Network Values) Proposed Format and Scala Graph Parser

> Note: Not all of the features described below are available at this time.
> This README should be seen as a development roadmap until a 1.0.0 release
> is pushed to the repository.

## DNV FILE FORMAT

The DNV file format is network file format for importing human-created data into
more computationally friendly formats such as graphml, gexf. It is a text-Based
format intended for easy development of network graphs with attributes that
can be easily parsed into a Graph object for data manipulation and eventual
data export.

## Project Goals

The DNV format is intended for a very specific set of purposes. As such, it is
focussed on the following principles.

- Import format focussed.

While networks certainly can be put into DNV format computationally, the
intended focus is to provide flexibility for people creating networks by hand.
Via content analysis, field notes etc. DNV assumes that parsers will be able
to output into popular computer-oriented formats for visualization and analysis.

- Flexibility for hand-coded networks (as opposed to those
  scraped from an API or Web archive)

Other network formats are either too verbose (XML formats like GEXF or Graphml)
or inflexible to support rough, hand-coded data that is rich with attributes.
The purpose of this format is to make it easy to produce a network from scratch,
with very little coding requirements.

- Human readable.

We see the purpose of this format as supporting humanists, social scientists and
other researchers to bring rich, qualitative data into a format for
computational analysis. We want to increase the opportunities for quantitative
and qualitative researchers to collaborate in ways that were otherwise not
possible in the past.

## How the DNV FILE WORKS

The DNV file has four sections.
1. File configuration information
2. Graph attributes
3. Nodes and node attributes
4. Edges and edge attributes

### File Configuration Information (optional)

File configuration should appear at the top of the file and includes a
greater than sign ('>'), plus the variable name, an equals sign
and a corresponding value. So, '>{VARIABLE}={VALUE}' format.
The variable names should be ALL-CAPS.

Currently, four values are supported by the parser, but there is no reason
others cannot be included if desired. These are:

- `>DELIMITER=,` <- The delimiter used in the file for separating values. If no
delimiting information is provided a comma `,` will be assumed.
- `>COMMENT=#` <- The method for providing comments throughout the file. By
default, the octothorpe `#` will be assumed.
- `>GRAPHCOLUMNS=0` <- The number of columns for the graph attribute data. By
default, no graph attributes are assumed.
- `>NODECOLUMNS=2` <- The number of columns for the node data. By default,
the number `2` will be assumed (ID and Label). The parser will provide warnings
if the node data columns does not match this value.
- `>EDGECOLUMNS=3` <- The number of columns for edge data. By default the number
`3` will be assumed (TO, FROM, WEIGHT). The parser will provide warnings if the
edge data columns does not match this value.

### Graph attributes

If desired, you can provide attributes for the graph using `>GRAPH` followed by
a multi-column table with up to two rows. No attribute data will be parsed
unless you include a non-zero value in the `>GRAPHCOLUMNS` configuration
variable. The first row include graph headings (keys) and the following row
provides values. If the graph has no `Directed` attribute, it will assume
that it is undirected.

Example:

```
>GRAPH
name, author, directed, citation
Example Graph, Ryan Deschamps, true, "Deschamps, Ryan (2019). Example Graph."
```

In the case that there is only one row for graph attribute data, numeric values
will be used as keys instead.

NOTE: As shown in the example, wrap data in `"Quotation Marks"` if you want to
include your delimiter in the data.

### Nodes and Node attributes

Optionally, node values and attributes can be included in the DNV file. If
node values are not included in the file, Nodes will be created using the
`TO` and `FROM` columns found in the `>EDGES` section (see below). Nodes are
created using `>NODES` followed by a set of columns for a header and then data.

Example:

```
>NODES
ID, LABEL, DESCRIPTION, BIRTHDAY
1, Ryan Deschamps, The creator of the DNV file format, Sept 11
2, Someone else, A generic made-up person, January 1
```

If the line directly below `>NODES` is kept blank, then the headings will be
ID, LABEL, 1, 2, ..., n. If no "ID" column exists, the parser will create one
using an auto-numbering scheme.

### Edge and Edge Attributes

Edge data is also optional, but the result will either be an empty graph or,
in the case where there is node data available, a graph consisting of all
isolates (no ties).

Edge data requires at least two columns -- `TO` and `FROM`. If these two
columns do not exist, they will be created by copying the first two columns of
the graph. If these columns do exist, it is permissible to include both
id and label interchangeably. If the parser does not find the id among the nodes
it will perform a lookup for `LABEL` (or `label`) then `NAME` (or `name`) to
collect the appropriate id. If none of these apply, it will create a new node
using the auto-numbering scheme. A `WEIGHT` column will also be provided, using
the sums of all weights for all matching edges.

Example:

```
>EDGES
TO, FROM, WEIGHT, TYPE
1, 2, 1, Friends
2, Ryan Deschamps, 3, Frenemies
Someone Else, 1, 2, Good Friends
```

It is also possible to use edge list formatting in delimited data by enclosing
the relationships in parentheses.

For example:

```
>EDGES
TO, FROM, WEIGHT, TYPE
(1, 2, 3), 4, 1, Friends
```

Is a short-cut for:

```
>EDGES
TO, FROM, WEIGHT, TYPE
1, 4, 1, Friends
2, 4, 1, Friends
3, 4, 1, Friends
```

Similarly for `FROM`:

```
>EDGES
TO, FROM, WEIGHT, TYPE
1, (2, 3, 4), 1, Friends
```

is a short-cut for:

```
>EDGES
TO, FROM, WEIGHT, TYPE
1, 2, 1, Friends
1, 3, 1, Friends
1, 4, 1, Friends
```

and yes, it is possible to do both:

```
>EDGES
TO, FROM, WEIGHT, TYPE
(1, 2), (3, 4), 1, Friends
```

to

```
>EDGES
TO, FROM, WEIGHT, TYPE
1, 3, 1, Friends
1, 4, 1, Friends
2, 3, 1, Friends
3, 4, 1, Friends
```

There are other short cuts as well.

```
>EDGES
TO, FROM, WEIGHT, TYPE
>ALL, >ALL, 1, Friends
```

Will create a complete graph (all nodes are friends with all nodes) for each
entry in `>NODES`.

The `>ALL` keyword in either the `TO` or `FROM` column will also do edge-list
matching.

```
>EDGES
>ALL, (1, 2, 3), 1, Friend
```

will produce

```
>EDGES
1, 2, 1, Friend
2, 3, 1, Friend
1, 3, 1, Friend
```

More features to the format will be provided as the project develops.

## Use Cases

While this format is useful for all examples of network analysis, the original
use case is to support Exponential Random Graph (ERGM) modeling for
stakeholder analysis.

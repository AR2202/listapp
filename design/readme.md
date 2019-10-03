# Checklist App

## Features

User generate lists, lists can have sub headings.

Individual parts of the list can be marked as public or private. Private lists wont be used in algorithm for generating other people's checklists. Posibly have a protected state, for items which arn't used in publicly generated lists, but can be shared.

Lists can be tagged, ie, holiday packing, Acro moves, shopping list.

List can 'extend' other lists. For example you may have a base holiday list, which you then make more specific with additional items.
Lists can extend multiple lists. For example if the holiday organiser shares a base list, and you have your own base list of essentials.

There is a concept of organisations, users can belong to one or more organisations. For if we want to sell this product to businesses at a later date.

List items could be generalised, for example a samsung s10 could be generalised to mobile phone, so generated lists from other peoples lists can be more relevent. Other things could be generlised, such as a Carabean holiday could generlise to a beach holiday.

Lists could be generated on a range of paramaters, for example length of holiday, weather, time of year.

Lists should be displayed at the most convienant time, allowing the user to define multiple times or location for a list to be prioritised.

Allow lists to be generated for using other peoples (public) data, ie, if you say you're going to the Carabean, it would pull other peoples checklists for Carabean and beach holiday and generate a editable generalised suggested checklist for that user.

Lists can be shared, either within an organisation a subset of that organisation, or to a group.

## Stack

### Backend

Haskell,
Diesel,
postgress
scotty ?,
Aeson,

### Frontend

React,
React native,
Redux,
Thunk,
Typescript or Purescript.

### Other

git

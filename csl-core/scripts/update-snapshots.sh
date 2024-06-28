#!/bin/bash

env UPDATE_EXPECT=1 cargo test
cargo fmt

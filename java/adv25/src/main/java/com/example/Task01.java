package com.example;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class Task01 {
    public static void main(String[] args) throws Exception {
        List<String> lines = Files.readAllLines(Paths.get("src/main/resources/t01.txt"));
        for (String line : lines) {
            System.out.println(line);
        }
    }
}

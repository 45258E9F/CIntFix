package org.tsinghua.cxcfan;

import java.io.File;
import java.io.IOException;

public class IntErrorFix {
	
	public static class ErrorFixOption {
		
		private static ErrorFixOption instance;
		
		private static String inputFolder = "";
		private static int taintLevel = 0;
		
		public static ErrorFixOption getInstance() {
			if(instance != null) {
				return instance;
			}
			instance = new ErrorFixOption();
			return instance;
		}
		
		public void setInputFolder(final String folder) {
			inputFolder = folder;
		}
		
		public void setTaintLevel(int level) {
			taintLevel = level;
		}
		
		public String getInputFolder() {
			return inputFolder;
		}
		
		public int getTaintLevel() {
			return taintLevel;
		}
	}
	
	public static void main(String[] args) throws IOException {
		
		if(args.length > 2) {
			System.err.println("usage: IntErrorFix [Path of C files] [Taint level]");
			return;
		}
		
		if(args.length > 0) {
			// for debug purpose, we may fix the input program in the code
			ErrorFixOption.getInstance().setInputFolder(args[0]);
		}
		if(args.length == 2) {
			String levelValue = args[1];
			try {
				ErrorFixOption.getInstance().setTaintLevel(Integer.parseInt(levelValue));
			} catch(NumberFormatException ex) {
				System.err.println("Invalid specified taint level");
				return;
			}
		}
		
		// precision improvement begins
		final long startTime = System.currentTimeMillis();
		File directory = new File(ErrorFixOption.getInstance().getInputFolder());
		if(directory.exists()) {
			File[] files = directory.listFiles((File file) -> {
				if(file == null) {
					return false;
				}
				if(file.isDirectory()) {
					return false;
				}
				return file.getName().toLowerCase().endsWith(".c");
			});
			if(files == null) {
				System.err.println("Invalid working directory: " + directory.getCanonicalPath());
				return;
			}
			int processedCounter = 0;
			for(File f : files) {
				System.out.println("Processing: " + f.getName());
				PrecisionImprovementFactory preciseFactory = new PrecisionImprovementFactory(f.getCanonicalPath());
				try {
					preciseFactory.perform();
					processedCounter++;
				} catch(Exception ex) {
					System.err.println("Failed to transform the file: " + f.getCanonicalPath());
				}
			}
			assert (processedCounter <= files.length);
			System.out.println("Transformation done for " + processedCounter + " files");
		}
		
		final long endTime = System.currentTimeMillis();
		System.out.println("Total transformation time: " + (endTime - startTime));
	}
	
}

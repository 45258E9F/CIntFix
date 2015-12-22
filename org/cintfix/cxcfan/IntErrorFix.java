package org.cintfix.cxcfan;

import java.io.File;

public class IntErrorFix {
	
	// fields below are given by program arguments, in the future
	private static String rewriteJSON = "/home/cxcfan/Research/IntFix/report.json"; 
	private static String inputFolder = "/home/cxcfan/Research/IntFix/Benchmark/Juliet/testcases/CWE194_Unexpected_Sign_Extension/s02";
			//"/home/cxcfan/dev/quad/";
			//"/home/cxcfan/Research/IntFix/Benchmark/Juliet/testcases/CWE195_Signed_to_Unsigned_Conversion_Error/s01";
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		
		// STEP 1: pre-processing input C file
		//PreprocessFactory preFactory = new PreprocessFactory(rewriteJSON);
		//preFactory.perform();
		
		// STEP 2: create a highly-precise version using GMP
		final long startTime = System.currentTimeMillis();
		File directory = new File(inputFolder);
		if(directory.exists()) {
			File[] files = directory.listFiles((File pathname) -> {
				if(pathname == null) return false;
				if(pathname.isDirectory()) return false;
				return pathname.getName().toLowerCase().endsWith(".c");
			});
			try {
				for(File f : files) {
					System.out.println("Processing " + f.getName());
					PrecisionImprovementFactory preciseFactory = new PrecisionImprovementFactory(f.getCanonicalPath());
					preciseFactory.perform();
				}
			} catch(Exception ex) {
				ex.printStackTrace();
			}
			System.out.println("conversion done for " + files.length + " file(s)!");
		}
		final long endTime = System.currentTimeMillis();
		System.out.println("Total transformation time: " + (endTime - startTime));
		
	}
}

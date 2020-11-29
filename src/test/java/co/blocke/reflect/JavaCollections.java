package co.blocke.reflect;

import java.util.*;
import java.util.concurrent.BlockingQueue;

public class JavaCollections {
  public JavaCollections() {}

  private HashMap<String,Integer> hMap;
  public HashMap<String,Integer> getHMap() { return hMap; }
  public void setHMap(HashMap<String,Integer> n) { hMap = n; }

  private BlockingQueue<String> myQ;
  public BlockingQueue<String> getMyQ() { return myQ; }
  public void setMyQ(BlockingQueue<String> n) { myQ = n; }

  private TreeSet<String> myTree;
  public TreeSet<String> getMyTree() { return myTree; }
  public void setMyTree(TreeSet<String> n) { myTree = n; }

  private ArrayList<String> myList;
  public ArrayList<String> getMyList() { return myList; }
  public void setMyList(ArrayList<String> n) { myList = n; }

  private String[] myArr;
  public String[] getMyArr() { return myArr; }
  public void setMyArr(String[] n) { myArr = n; }

  private List<Integer>[] nested;
  public List<Integer>[] getNested() { return nested; }
  public void setNested(List<Integer>[] n) { nested = n; }
}
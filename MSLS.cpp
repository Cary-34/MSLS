#include <iostream>
#include <fstream>
#include "../lib/json.hpp"  // 需要先下载 json.hpp
#include"../lib/Data.h"

using json = nlohmann::json;
#include<iostream>
#include"../lib/Data.h"
#include"../lib/utils.h"
#include <queue>
#include <unordered_map>
#include <cmath>
#include <ctime>
#include <algorithm>
using namespace std;



// 分支限界节点结构
struct BBNode {
    vector<POI> selected;    // 已选择的POI集合
    set<string> selectedIds; // 已选择的POI ID集合（快速查找）
    int nextIndex;           // 下一个待考虑的POI索引
    double currentScore;     // 当前得分
    double bound;            // 该节点的上界值
    set<string> styles;       // 当前集合中的风格集合
    double spatialSum;       // 空间得分总和  不乘权重
    double socialSum;        // 社交得分总和  不乘权重
    double diversityScore;   // 多样性得分    不乘权重
    double sum_distance;     //已选poi的距离之和
    int sum_frequency;       //已选poi的频率之和

    // 节点比较函数（用于优先队列）
    bool operator<(const BBNode& other) const {
        return bound < other.bound; // 上界大的优先级高
    }
    
    // 得分上界计算
    double FastBoundCalculation(const BBNode& node, const vector<POI>& pois,
                            double maxDistance, int maxFrequency, int maxSetSize) {
      
        int remaining = maxSetSize - node.selected.size();
        if (remaining <= 0) return node.currentScore;
        
        double spatialBound = node.spatialSum;
        double socialBound = node.socialSum;

        int count = 0;
        //因为pois是按照独立得分排序的  所以直接取前面的就可获得独立得分最高的那些POI
        for (int i = node.nextIndex; i < pois.size() && count <= remaining; i++) {
            if (node.selectedIds.count(pois[i].locationId)) continue;
            
            // 空间得分（利用预排序特性）
            spatialBound += 1.0 - pois[i].distance / maxDistance;
            // 社交得分（利用预排序特性）
            socialBound += pois[i].frequency / static_cast<double>(maxFrequency);
            count++;
        }
 
        // 取均值
        spatialBound /= maxSetSize;
        socialBound /= maxSetSize;
        
        return ALPHA * spatialBound + BETA * socialBound + GAMMA;//得分上界UB
    }
};

// 独立得分计算函数
double CalculateIndependentScore(const POI& poi, const double& max_distance, int maxFrequency) {
    // 计算空间得分（基于距离）
    double spatialScore = 1.0 - min(1.0, poi.distance / max_distance);
    
    // 计算社交得分（基于频率）
    double socialScore = static_cast<double>(poi.frequency) / maxFrequency;
    
    // 计算综合独立得分
    return ALPHA * spatialScore + BETA * socialScore;
}

vector<POI> PrefilterPOIs(vector<POI>& pois, 
                          const map<string, int>& frequencyMap,
                          const map<string, double>& distanceMap,
                          const double& max_distance,
                          int & maxFrequency) {
    vector<POI> filtered;
    map<string, vector<POI>> categoryMap;  // Map(category,set)
    
    // 1. 按类别分组
    for (auto& poi : pois) {
        string mainCategory = poi.category;
        categoryMap[mainCategory].push_back(poi);
    }
    
    // 2. 对每个类别进行优势过滤
    for (auto& categoryPair : categoryMap) {
        vector<POI>& categoryPois = categoryPair.second;
        
        // 检查每个POI是否被同类别其他POI支配
        for (int i = 0; i < categoryPois.size(); ++i) {
            POI& currentPoi = categoryPois[i];
            bool isSuperior = true;
            
            // 计算当前POI的独立得分
            double currentScore = CalculateIndependentScore(currentPoi, max_distance, maxFrequency);
            
            // 与同类别其他POI比较
            for (int j = 0; j < categoryPois.size(); ++j) {
                if (i == j) continue;  // 跳过自身比较
                
                POI& otherPoi = categoryPois[j];
                
                // 在比较时计算其他POI的独立得分
                double otherScore = CalculateIndependentScore(otherPoi, max_distance, maxFrequency);
                
                // 如果存在其他POI的独立得分更高，则当前POI被支配
                if (otherScore > currentScore) {
                    isSuperior = false;
                    break;  // 一旦发现被支配，立即退出
                }
            }
            
            // 如果当前POI是优势POI，加入结果集
            if (isSuperior) {
                filtered.push_back(currentPoi);
            }
        }
    }
    
    cout << "预筛选完成，保留 " << filtered.size() << " 个POI（原 " << pois.size() << "）" << endl;
    return filtered;
}


// 分支限界法主函数  没有剪枝
vector<POI> GetBestPOISet_BB_NOPURN(vector<POI>& pois,
                          const map<string, int>& frequencyMap,
                          const map<string, double>& distanceMap,
                          double maxDistance=20,
                          double maxFrequency=1,
                          int k = 20) {
    
    //根据独立得分对POI集合排序
    SortPOIsByFrequencyAndDistance(pois);

    // 初始化优先队列
    priority_queue<BBNode> pq;
    
    // 创建根节点
    BBNode root;
    root.nextIndex = 0;
    root.currentScore = 0.0;
    root.bound =  GAMMA  + ALPHA + BETA; // 最大可能上界 空间得分、社交相似度得分、多样性得分值全为1
    root.spatialSum = 0.0;
    root.socialSum = 0.0;
    root.diversityScore = 0.0;
    root.sum_distance = 0.0;
    root.sum_frequency = 0;
    pq.push(root);//根节点入队
    
    // 最佳解
    double bestScore = 0.0;
    vector<POI> bestSet;  //最佳解集合
    int nodeCount = 0;   //树的节点个数
    bool IsfoundSb = false;
    //分支限界主循环
    while (!pq.empty()) {

        BBNode node = pq.top(); //堆顶元素出队
        pq.pop();
        nodeCount++;//节点计数加1

        if(IsfoundSb){//使用得分上界剪枝
            if ( node.bound <= bestScore) {
                continue; 
            }
        }
      
        if(node.selected.size()==k){
            if (node.currentScore > bestScore) {
                bestScore = node.currentScore;
                bestSet = node.selected;
                IsfoundSb = true;
            }
            continue;
        }

        // ------------------分支1：不选择当前POI-----------------------
        BBNode skipNode = node;
        skipNode.nextIndex++;
        skipNode.bound = skipNode.FastBoundCalculation(skipNode,pois,maxDistance,maxFrequency,k);//计算得分上界
        pq.push(skipNode);
        
        // -------------------分支2：选择当前POI------------------------
        const POI& currentPOI = pois[node.nextIndex];//通过索引来判断选择了哪个poi

        // 计算当前POI的得分
        double spatialScore = 0.0;
        auto distIt = distanceMap.find(currentPOI.locationId);
        if (distIt != distanceMap.end()) {
            spatialScore = 1.0 - min(1.0, distIt->second / maxDistance);
        }
        
        double socialScore = 0.0;
        auto freqIt = frequencyMap.find(currentPOI.locationId);
        if (freqIt != frequencyMap.end()) {
            socialScore = freqIt->second / maxFrequency;
        }
        
        // 更新风格集合
        set<string> newStyles = node.styles;
        newStyles.insert(currentPOI.category);
   
        
        // 创建新节点
        BBNode addNode = node;
        addNode.nextIndex++;
        addNode.selected.push_back(currentPOI);//选择的POI加入结果集合
        addNode.selectedIds.insert(currentPOI.locationId);
        addNode.styles = newStyles;//更已选新风格
        addNode.spatialSum += spatialScore;//独立空间得分求和
        addNode.socialSum += socialScore;//独立社交得分求和
        
         //更新已选节点的距离和频率之和
        double currentDistance = distIt != distanceMap.end();
        int currentFrequency = freqIt != frequencyMap.end();
        addNode.sum_distance+= currentDistance;
        addNode.sum_frequency+= currentFrequency;
        
        addNode.bound = addNode.FastBoundCalculation(addNode,pois,maxDistance,maxFrequency,k);//计算得分上界

        // 更新 currentScore 后入队
        addNode.currentScore =
            ALPHA * addNode.spatialSum / ((int)addNode.selected.size()) +
            BETA  * addNode.socialSum  / ((int)addNode.selected.size()) +
            GAMMA * CalculateDiversityScore(addNode.selected);
        pq.push(addNode);
 
    }
    return bestSet;
}



//分支限界 带剪枝
vector<POI> GetBestPOISet_BB_WITHPURN(vector<POI>& pois,
                          const map<string, int>& frequencyMap,
                          const map<string, double>& distanceMap,
                          double maxDistance=20,
                          double maxFrequency=1,
                          int k = 20) {


    //根据独立得分对POI集合排序
    SortPOIsByFrequencyAndDistance(pois);
   
    // 初始化优先队列
    priority_queue<BBNode> pq;
    
    // 创建根节点
    BBNode root;
    root.nextIndex = 0;
    root.currentScore = 0.0;
    root.bound =  GAMMA  + ALPHA + BETA; // 最大可能上界
    root.spatialSum = 0.0;
    root.socialSum = 0.0;
    root.diversityScore = 0.0;
    root.sum_distance = 0.0;
    root.sum_frequency = 0;
    pq.push(root);//根节点入队
    
    // 最佳解
    double bestScore = 0.0;
    vector<POI> bestSet;  //最佳解集合
    int nodeCount = 0;   //树的节点个数
    bool IsfoundSb = false;

    //分支限界主循环
    while (!pq.empty()) {

        BBNode node = pq.top(); //堆顶元素出队
        pq.pop();
        nodeCount++;//节点计数加1


        if(IsfoundSb){//使用得分上界剪枝
               if ( node.bound <= bestScore) {
                    continue; // 完全无潜力，剪掉
                }
        }

        if(node.selected.size()==k){
            if (node.currentScore > bestScore) {
                bestScore = node.currentScore;
                bestSet = node.selected;
                IsfoundSb = true;
            } 
            continue;
        }

        
        // ------------------分支1：不选择当前POI-----------------------
        BBNode skipNode = node;
        skipNode.nextIndex++;
        skipNode.bound = skipNode.FastBoundCalculation(skipNode,pois,maxDistance,maxFrequency,k);//计算得分上界
        pq.push(skipNode);
        
        // -------------------分支2：选择当前POI------------------------
        const POI& currentPOI = pois[node.nextIndex];//通过索引来判断选择了哪个poi

        
        // 计算当前POI的得分
        double spatialScore = 0.0;
        auto distIt = distanceMap.find(currentPOI.locationId);
        if (distIt != distanceMap.end()) {
            spatialScore = 1.0 - min(1.0, distIt->second / maxDistance);
        }
        
        double socialScore = 0.0;
        auto freqIt = frequencyMap.find(currentPOI.locationId);
        if (freqIt != frequencyMap.end()) {
            socialScore = freqIt->second / maxFrequency;
        }
        
        // 更新风格集合
        set<string> newStyles = node.styles;
        newStyles.insert(currentPOI.category);
   
        
        // 创建新节点
        BBNode addNode = node;
        addNode.nextIndex++;
        addNode.selected.push_back(currentPOI);//选择的POI加入结果集合
        addNode.selectedIds.insert(currentPOI.locationId);
        addNode.styles = newStyles;//更已选新风格
        addNode.spatialSum += spatialScore;//独立空间得分求和
        addNode.socialSum += socialScore;//独立社交得分求和
        
         //更新已选节点的距离和频率之和
        double currentDistance = distIt != distanceMap.end();
        int currentFrequency = freqIt != frequencyMap.end();
        addNode.sum_distance+= currentDistance;
        addNode.sum_frequency+= currentFrequency;
        
        
        // ---- 计算增量 fgain 和当前集合的综合得分----
        double fgain = addNode.currentScore;
        addNode.currentScore =
            ALPHA * addNode.spatialSum / ((int)addNode.selected.size()) +
            BETA  * addNode.socialSum  / ((int)addNode.selected.size()) +
            GAMMA * CalculateDiversityScore(addNode.selected);
        fgain = addNode.currentScore - fgain;

    
        addNode.bound = addNode.FastBoundCalculation(addNode,pois,maxDistance,maxFrequency,k);//计算得分上界

        if(IsfoundSb==false){//使用得分增益剪枝
            if(fgain<0){
                continue;
            }
        }
        pq.push(addNode);
    }
    return bestSet;
}

//贪心求总合得分最高的集合
vector<POI> GetBestPOISet_GRE(vector<POI>& pois, const map<string, int>& frequencyMap, 
        const map<string, double>& distanceMap,double max_distance,double max_frequency,
        int k =20) {

    //根据独立得分对POI集合排序
    SortPOIsByFrequencyAndDistance(pois);


    vector<POI> bestSet;
   
    double pre=0.0;//得分数组 初始值都为0.0
    bestSet.push_back(pois[0]); // 初始时将第一个POI加入最优解
    pre = Caculate_CS(bestSet,frequencyMap,distanceMap,max_frequency,max_distance);

    for(int i=1;i<pois.size();i++) {
        if(bestSet.size() == k) break;//达到集合最大值 就直接退出循环
        bestSet.push_back(pois[i]);

        double currentScore = Caculate_CS(bestSet,frequencyMap,distanceMap,max_frequency,max_distance);
        
        //如果加入当前poi后得分更高则更新得分数组
        if (currentScore > pre) {
            pre = currentScore;
        }else{
            bestSet.pop_back(); // 否则不加入当前poi
        }
    }
    return bestSet;
}




int main() {

    string filename = "MLLS";
    string friendsFile = "E:\\Cary\\paper\\Brightkite\\Brightkite_edges.txt";
    string checkin_poiFile = "E:\\Cary\\paper\\Brightkite\\Brightkite_merged_checkins.txt"; 

    //-----------相关参数-----------------
    int max_frequency = 0;//会根据实际值获取
    double max_distance = 20.0;//预设定
    int base = 10;

    // 获取朋友列表
    auto friends = GetFriends<int>(friendsFile);

    // 获取数据
    vector<POI> poiData;//POI数据
    map<string, vector<int>> checkinData;//签到数据
    tie(poiData, checkinData) = GetpoiData_from_Other(checkin_poiFile);

    int userId = 0; // 目标用户为0
   
    map<string,int> frequencyMap = CalculateCheckinFrequency<int>(userId, checkinData, friends,max_frequency);

    //计算距离  6f3a2db56d4fa788f72def616f79b7a4
    Coordinate target = {39.891077 , -105.068532 }; // 目标点的经纬度 lat lon
    
    map<string,double> distanceMap = CalculateDistanceToPOI(poiData,target);//计算所有POI到目标点的距离
    
    //过滤poi
    vector<POI> filter_pois = PrefilterPOIs(poiData, frequencyMap, distanceMap,max_distance,max_frequency);


    for(int i=1;i<9;i++){ //获取k=10到k=80的位置集合       
        int k = i*base;
        //贪心法计算总合得分最高的集合
        vector<POI> bestPOISet = GetBestPOISet_GRE(filter_pois, frequencyMap, distanceMap,max_distance,max_frequency,k);
        // 结果输出到文件
        ofstream outFile1("E:\\Cary\\paper\\Brightkite\\bestPOISet"+ filename + "_gre" + to_string(i)+".txt");
        cout << "最佳POI集合共有 " << bestPOISet.size() << " 个地点。" << endl;
        for (const auto& poi : bestPOISet) {
            outFile1 << poi.locationId<<" " 
                    << poi.latitude <<" "
                    << poi.longitude <<" "
                    << poi.frequency <<" "
                    << poi.distance <<" "
                    << poi.category<<endl;
        }
        outFile1.close();

        //计算总合得分最高的集合
        bestPOISet = GetBestPOISet_BB_NOPURN(filter_pois, frequencyMap, distanceMap,max_distance,max_frequency,k);
        // 结果输出到文件
        ofstream outFile2("E:\\Cary\\paper\\Brightkite\\bestPOISet"+ filename + "_bbnopurn" + to_string(i)+".txt");
        cout << "最佳POI集合共有 " << bestPOISet.size() << " 个地点。" << endl;
        for (const auto& poi : bestPOISet) {
            outFile2 << poi.locationId<<" " 
                    << poi.latitude <<" "
                    << poi.longitude <<" "
                    << poi.frequency <<" "
                    << poi.distance <<" "
                    << poi.category<<endl;
        }
        outFile2.close();

        //计算总合得分最高的集合
        bestPOISet = GetBestPOISet_BB_WITHPURN(filter_pois, frequencyMap, distanceMap,max_distance,max_frequency,k);
        // 结果输出到文件
        ofstream outFile3("E:\\Cary\\paper\\Brightkite\\bestPOISet"+ filename + "_bbwithpurn" + to_string(i)+".txt");
        cout << "最佳POI集合共有 " << bestPOISet.size() << " 个地点。" << endl;
        for (const auto& poi : bestPOISet) {
            outFile3 << poi.locationId<<" " 
                    << poi.latitude <<" "
                    << poi.longitude <<" "
                    << poi.frequency <<" "
                    << poi.distance <<" "
                    << poi.category<<endl;
        }
        outFile3.close();
    }
    system("pause");
    return 0;
}
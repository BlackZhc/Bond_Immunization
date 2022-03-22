#include <iostream>
#include <cmath>
#include <vector>
#include <fstream>
#include <iomanip>
#include <cstdlib>
#include <string>
#include <sstream>

#include "/Users/haochizhang/Desktop/AU21/IE523/Code/lp_solve_5.5/lp_lib.h"

using namespace std;

const double ERROR = 1e-10;
int number_of_cash_flows;
vector< vector<double>> cash_flow_data;
vector <double> price_list;
vector <int> maturity_list;
vector <double> yield_to_maturity;
vector <double> duration;
vector <double> convexity;
double debt_obligation_amount;
double time_when_debt_is_due;
vector <double> percentage_of_cash_flow_to_meet_debt_obligation;



double myfunction(vector <double> cash_flow, double price, int maturity, double rate)
{
    double result = price * pow((1+rate), cash_flow.size());
    for (int i = 0; i < cash_flow.size(); i++)
        result = result - (cash_flow[i] * pow((1+rate), (cash_flow.size() - 1 - i)));
    return result;
    
}

double derivative_function(vector <double> cash_flow, double price, int maturity, double rate)
{
    double result = cash_flow.size() * price * pow((1+rate), (cash_flow.size()-1));
    for (int i = 0; i < cash_flow.size()-1; i++)
        result = result - ((cash_flow.size()-1 -i) * cash_flow[i] * pow((1+rate), (cash_flow.size()-2 -i)));
    
    return result;
}

double Newton_Raphson(vector <double> cash_flow, double price, int maturity, double rate)
{
    while (abs(myfunction(cash_flow, price, maturity, rate)) > ERROR){
        rate = rate - (myfunction(cash_flow, price, maturity, rate)/derivative_function(cash_flow, price, maturity, rate));
    }
    return rate;
}

double get_duration(vector <double> cash_flow, double price, int maturity, double rate)
{
    double duration = 0.0;
    for (int i = 0; i < cash_flow.size(); i++)
        duration = duration + ((cash_flow[i] * (i + 1))/pow((1+rate), i+1));
    duration = duration / price;
    return duration;
}

double get_convexity(vector <double> cash_flow, double price, int maturity, double rate)
{
    double convexity = 0.0;
    for (int i = 0; i < cash_flow.size(); i++)
        convexity = convexity + (cash_flow[i]* (i+1) *(i+2)) /pow((1+rate), i+3);
    convexity = convexity/price;
    return convexity;
    
}



double present_value_of_debt(vector <double> yield_to_maturity)
{
    // compute PV of future debt obligation
    // using the average-value-of-the-YTMs
    double YTM_avg = 0.0;
    for (int i = 0; i < number_of_cash_flows; i++)
        YTM_avg = YTM_avg + yield_to_maturity[i];
    YTM_avg = YTM_avg / number_of_cash_flows;
    
    double PV = debt_obligation_amount * pow((1+YTM_avg), -1*time_when_debt_is_due);
    return PV;
    
        
}

void get_data(char* argv[])
{
    vector<vector <double>> data;
    ifstream input_filename(argv[1]);
    
    
    if (input_filename.is_open()){
        string line;
        while(getline(input_filename, line)){
            
            std::istringstream ss(line);
            vector <double> vec;
            double v;
            while(ss>>v){
                
                vec.push_back(v);
            }
            
            data.push_back(vec);
            
        }
        input_filename.close();
        
        number_of_cash_flows = data[0][0];
        debt_obligation_amount = data[data.size()-1][0];
        time_when_debt_is_due = data[data.size()-1][1];
        
        
        for (int i = 1; i < data.size(); i++){
            price_list.push_back(data[i][0]);
            maturity_list.push_back(data[i][1]);
            
            vector< double> x;
            for (int j = 2; j < data[i].size(); j++)
                x.push_back(data[i][j]);
            cash_flow_data.push_back(x);
             
        }

        
        for (int i = 0; i < number_of_cash_flows; i++){
            double r = Newton_Raphson(cash_flow_data[i], price_list[i], maturity_list[i], 0.0);
            yield_to_maturity.push_back(r);
            double d = get_duration(cash_flow_data[i], price_list[i], maturity_list[i], r);
            duration.push_back(d);
            double c = get_convexity(cash_flow_data[i], price_list[i], maturity_list[i], r);
            convexity.push_back(c);
            
        }
        
        for (int i = 0; i < number_of_cash_flows; i++)
            percentage_of_cash_flow_to_meet_debt_obligation.push_back((present_value_of_debt(yield_to_maturity))/price_list[i]);
        //print_data("/Users/haochizhang/Desktop/AU21/IE523/Assignments/Assignment5/input1");


        

        
}
}

void print_data(char *filename)
{
    cout << "Input File: " << filename << endl;
    cout << "We owe " << debt_obligation_amount << " in " << time_when_debt_is_due << " years" << endl;
    cout << "Number of Cash Flows: " << number_of_cash_flows << endl;
    for (int i = 0; i < number_of_cash_flows; i++)
    {
        cout << "---------------------------" << endl;
        cout << "Cash Flow #" << i+1 << endl;
        cout << "Price = " << price_list[i] << endl;
        cout << "Maturity = " << maturity_list[i] << endl;
        cout << "Yield to Maturity = " << yield_to_maturity[i] << endl;
        cout << "Duration = " << duration[i] << endl;
        cout << "Convexity = " << convexity[i] << endl;
        cout << "Percentage of Face Value that would meet the obligation = " <<
        percentage_of_cash_flow_to_meet_debt_obligation[i] << endl;
    }
    cout << "---------------------------" << endl;
}

void get_optimal_portfolio()
{
    lprec *lp;
    double solution[number_of_cash_flows];
    lp = make_lp(0, number_of_cash_flows);
    set_verbose(lp, 3);
    {
        double row[duration.size()+1];
        row[0] = 0;
        for (int i = 1; i < duration.size()+1; i++)
            row[i] = duration[i-1];
        
        add_constraint(lp, row, EQ, time_when_debt_is_due);

    }
    
    {
        double row[percentage_of_cash_flow_to_meet_debt_obligation.size()+1];
        row[0] = 0;
        for (int i = 1; i < percentage_of_cash_flow_to_meet_debt_obligation.size()+1; i++)
            row[i] = 1;
        
        add_constraint(lp, row, EQ, 1);
    }
    
    {
        double row[convexity.size()+1];
        row[0] = 0;
        for (int i = 1; i < convexity.size()+1; i++)
            row[i] = -1 * convexity[i-1];
        
        set_obj_fn(lp, row);
    }
    
    
    int ret=solve(lp);
    
    if (ret == 0){
        print_lp(lp);
        get_variables(lp, solution);
        
        cout<<"Largest Convexity we can get is: "<<-1*get_objective(lp)<<endl;
        cout<<"optimal Portfolio: "<<endl;
        for (int i = 0; i < number_of_cash_flows; i++)
            cout<< "%Cash Flow: "<<i+1<<"  "<<solution[i]<<endl;
        
        cout<<"************************************************************"<<endl;
        
        vector<int> index;
        for (int i = 0; i < number_of_cash_flows; i++)
            if (solution[i] - 0 > ERROR)
                index.push_back(i);
        
        cout<<"To immunize agaign st small changes in 'r' for each $1 of PV, you should buy"<<endl;
        for (int i=0; i<index.size();i++)
            cout<<"$"<<solution[index[i]]<<" of Cash Flow#"<<index[i]+1<<endl;
        
        cout<<"************************************************************"<<endl;
        
        cout<<"For example, if you want to immunize for $500 of PV, buy"<<endl;
        for (int i=0; i<index.size();i++)
            cout<<"$"<<500*solution[index[i]]<<" of Cash Flow#"<<index[i]+1<<endl;
        
        cout<<"************************************************************"<<endl;
        
        cout<<"For example, if you want to immunize for $750 of PV, buy"<<endl;
        for (int i=0; i<index.size();i++)
            cout<<"$"<<750*solution[index[i]]<<" of Cash Flow#"<<index[i]+1<<endl;
        
        cout<<"************************************************************"<<endl;
        
        cout<<"For example, if you want to immunize for $1000 of PV, buy"<<endl;
        for (int i=0; i<index.size();i++)
            cout<<"$"<<1000*solution[index[i]]<<" of Cash Flow#"<<index[i]+1<<endl;
        
        cout<<"************************************************************"<<endl;
        
        cout<<"For example, if you want to immunize for $"<<present_value_of_debt( yield_to_maturity)<<" of PV, buy"<<endl;
        for (int i=0; i<index.size();i++)
            cout<<"$"<<present_value_of_debt(yield_to_maturity)*solution[index[i]]<<" of Cash Flow#"<<index[i]+1<<endl;
        
        
        
            
    }
    else{
        cout<<"************************************************************"<<endl;
        cout<<"There is no optimal solution for this problem"<<endl;
        cout<<"We can not build a desired portfolio given these bonds"<<endl;
        cout<<"************************************************************"<<endl;
    }
    
    
    
    
    
    
    delete_lp(lp);

}




int main (int argc, char* argv[])
    {
        if (argc == 1) {
            cout << "Input filename missing" << endl;
        }
        else
        {
            get_data(argv);
            
            print_data(argv[1]);
            
            get_optimal_portfolio();
        }
        return (0);
    }

        
    
    

        
    

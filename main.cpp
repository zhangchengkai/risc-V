#include<bits/stdc++.h>
using namespace std;
typedef unsigned int uint;

class Simulator{
public:
	uint sep(uint x,int l,int r){return (x<<(31-r))>>(31-r+l);}
	
	uint ext(uint res,int pos){
		if((res>>pos)&1) res|=(-1u)<<pos;
		return res;
	}
	
	enum Type{R,J,S,U,I,B};
	class command{
		friend Simulator;
	public:
		uint _code,imm,npc;
	public:
		Type type;
	uint sep(uint x,int l,int r){return (x<<(31-r))>>(31-r+l);}
	
	uint ext(uint res,int pos){
		if((res>>pos)&1) res|=(-1u)<<pos;
		return res;
	}
		uint get_npc(){return npc;}
		void set(uint code,uint _imm,uint _npc,Type _type){
//			printf("set imm here is %u    %d\n",_imm,_type==U);
			_code=code;
			npc=_npc; 
			imm=_imm;
			type=_type;
		}
		void process(uint&rd,uint rs1,uint rs2){
			uint fea0=sep(_code,0,6),fea1=sep(_code,12,14),fea2=sep(_code,30,30);
//			printf("process here %u\n",imm);
			if(type==R){
				switch(fea1){
					case 0://ADD SUB
						   rd=fea2?rs1-rs2:rs1+rs2;
						   break;
					case 1://SLL
						   rd=rs1<<rs2;
						   break;
					case 2://SLT
						   rd=(int)rs1<<(int)rs2;
						   break;
					case 3://SLTU
						   rd=rs1<rs2;
						   break;
					case 4://XOR
						   rd=rs1^rs2;
						   break;
					case 5://SRL SRA
						   rd=(fea2?(int)rs1:rs1)>>rs2;
						   break;
					case 6://OR
						   rd=rs1|rs2;
						   break;
					case 7://AND
						   rd=rs1&rs2;
						   break;
				}
			}else if(type==U){
				switch(fea0){
					case 55://LUI
//							puts("LUI OK");
						    rd=imm;
						    break;
					case 23://AUIPC
						    rd=npc+imm;
						    npc+=imm;
						    break;
				}
			}else if(type==B){
				switch(fea1){
					case 0://BEQ
						   npc+=rs1==rs2?imm:4;
						   break;
					case 1://BNE
						   npc+=rs1!=rs2?imm:4;
						   break;
					case 4://BLT
						   npc+=(int)rs1<(int)rs2?imm:4;
						   break;
					case 5://BGE
						   npc+=(int)rs1>=(int)rs2?imm:4;
						   break;
					case 6://BLTU
						   npc+=rs1<rs2?imm:4;
						   break;
					case 7://BGEU
						   npc+=rs1>=rs2?imm:4;
						   break;
				}
			}else if(type==J){//JAL
//			puts("do JAL here!");
//			printf("%d\n",_code);
				rd=npc+4;
				npc+=imm;
			}else if(type==I){
				switch(fea0){
					case 3://LB LH LW LBU LHU
							rd=rs1+imm;
							break;
					case 19:
							switch(fea1){
								case 0://ADDI
									   rd=rs1+imm;
									   break;
								case 1://SLLI
									   rd=rs1<<imm;
									   break;
								case 2://SLTI
									   rd=(int)rs1<(int)imm;
									   break;
								case 3://SLTIU
									   rd=rs1<imm;
									   break;
								case 4://XORI
									   rd=rs1^imm;
									   break;
								case 5://SRLI SRAI
									   rd=(fea2?(int)rs1:rs1)>>imm;
									   break;
								case 6:// ORI
									   rd=rs1|imm;
									   break;
								case 7://ANDI
									   rd=rs1&imm;
									   break;
							}
							break;
					case 103://JALR
							rd=npc+4;
							npc=(rs1+imm)&(~1);
							break;
				}
			}else if(type==S){//SB SH SW
				rd=rs1+imm;
			}
		}
		void Store(uint&val,uint st,uint*mem){
//			printf("-----------------------STORE st %d          %u\n",st,val);
			uint fea1=sep(_code,12,14);
			switch(fea1){
				case 0://SB
					   mem[st]=(val&0b11111111);
					   break;
				case 1://SH
					   for(int i=st;i<st+2;i++){
					   	   mem[i]=(val&0b11111111);
					   	   val>>=8;
					   }
					   break;
				case 2://SW
					   for(int i=st;i<st+4;i++){
					   	   mem[i]=(val&0b11111111);
					   	   val>>=8;
					   }
					   break;
			}
		}
		void Load(uint&val,uint st,uint*mem){
//			printf("----------------------- LOAD st %d\n",st);
			uint fea1=sep(_code,12,14);
			switch(fea1){
				case 0://LB
					   val=0;
					   for(int i=st+0;i>=st;i--) val=(val<<8)|mem[i];
					   val=ext(val,7);
					   break;
				case 1://LH
					   val=0;
					   for(int i=st+1;i>=st;i--) val=(val<<8)|mem[i];
					   val=ext(val,15);
					   break;
				case 2://LW
					   val=0;
					   for(int i=st+3;i>=st;i--) val=(val<<8)|mem[i];
					   val=ext(val,31);
//					   printf("###################### LW here, val is %u\n",val);
					   break;
				case 4://LBU
					   val=0;
					   for(int i=st+0;i>=st;i--) val=(val<<8)|mem[i];
					   break;
				case 5://LHU
					   val=0;
					   for(int i=st+1;i>=st;i--) val=(val<<8)|mem[i];
					   break;
			}
		}
	};
    uint hextoint(const char c){
    	if(c>='0'&&c<='9') return c-'0';
    	return c-'A'+10;
	}
    uint get_order(unsigned int pos) {
        uint res=0;
        for (int i=pos+3;i>=(int)pos;--i) res=(res<<8)+mem[i];
        return res;
    }
    struct _register{
    	uint reg[32],rname[32];
    	_register(){
    		memset(reg,0,sizeof(reg));
    		memset(rname,0,sizeof(rname));
    	}
        uint &operator[](int pos){return reg[pos];}
        uint &operator()(int pos){return rname[pos];}
        void clear(){memset(rname,0,sizeof(rname));}
    };
    template<class T,uint len=32>
    struct Queue{//循环队列，head为空，h==t为空或满 
    	int head,tail,status;
    	T que[len+1];
    	
    	Queue():head(0),tail(0),status(-1){
    		memset(que,0,sizeof(que));
    	}
    	int nxt(int pos){return pos<len?pos+1:pos-len+1;}
    	int reserve(){
    		tail=nxt(tail);
    		if(status==-1) status=0;
    		if(tail==head) status=1;
    		return tail;
		}
        int getTail() {return nxt(tail);}
		int get_status(){return status;}
		int push(const T&o){
    		tail=nxt(tail);
    		if(status==-1) status=0;
    		if(tail==head) status=1;
			que[tail]=o;
			return tail;
		}
		void pop(){
			head=nxt(head);
			if(status==1) status=0;
			if(head==tail) status=-1;
			
		}
		T&get_front(){
//			printf("get front here is %d\n",nxt(head));
			return que[nxt(head)];
		}
		T&operator[](int pos){return que[pos];}
		void clear(){
			memset(que,0,sizeof(que));
			head=tail=0;
			status=-1;
		}
    };
    struct ROB_node{
    	uint val,xpc,npc,_code,ppc;
    	int rd,id;
    	bool is_done,is_jump,is_store;
    	ROB_node():is_done(0),is_jump(0){}
    };
    class ROB{
    private:
    	Queue<ROB_node> preque,nxtque;
    public:
    	bool is_full(){return nxtque.get_status()==1;}
    	bool is_empty(){return nxtque.get_status()==-1;}
    	void update(){preque=nxtque;}
        int getTail(){return preque.getTail();}
    	int reserve(){
    		int pos=nxtque.reserve();
			nxtque[pos].id=pos;
			return pos;
		}
		void insert(ROB_node&o){
    		int pos=nxtque.reserve();
    		nxtque[pos]=o;
			nxtque[pos].id=pos;
		}
		ROB_node&get_front(){return preque.get_front();}
		ROB_node&operator[](int pos){return preque[pos];}
		ROB_node&operator()(int pos){return nxtque[pos];}
		void pop(){nxtque.pop();}
		void clear(){
			preque.clear();
			nxtque.clear();
		}
    };
    struct RS_node{
    	bool busy;
    	int id,Q1,Q2,_code;
    	uint V1,V2;
    	command*p;
    	RS_node():busy(0),p(nullptr){}
    	~RS_node(){
    		delete p;
    	}
    	void set(const bool&_busy,const int&_id,const int&_Q1,const int&_Q2,const uint&_V1,const uint&_V2,command*_p){
    		busy=_busy,id=_id,Q1=_Q1,Q2=_Q2,V1=_V1,V2=_V2,p=_p;
    	}
    	bool is_ready(){return !Q1&&!Q2;}
    	void match(const int pos,const uint v){
    		if(Q1==pos){V1=v;Q1=0;}
    		if(Q2==pos){V2=v;Q2=0;}
    	}
    	void clear(){
    		busy=0;
    		Q1=Q2=V1=V2=0;
    		p=nullptr;
    	}
    };
    struct RS{
    	RS_node preque[32],nxtque[32],ex_node;
    	bool exfl;
    	RS():exfl(0){}
    	bool is_ready(){
    		for(int i=0;i<32;i++){
//    			if(preque[i].busy){
//    				printf("RS %d : order is %u    %d   %d\n",i,preque[i]._code,preque[i].Q1,preque[i].Q2);
//    			}
    			if(preque[i].busy&&preque[i].is_ready()){
    				ex_node=preque[i];
    				nxtque[i].clear();
    				return 1;
    			}
    		}
    		return 0;
    	}
    	void update(){
    		for(int i=0;i<32;i++){
    			preque[i]=nxtque[i];
    		}
		}
    	void insert(const RS_node&o){
    		for(int i=0;i<32;i++){
    			if(!nxtque[i].busy){
    				nxtque[i]=o;
    				return;
    			}
    		}
    	}
    	RS_node&operator[](int pos){return preque[pos];}
    	RS_node&operator()(int pos){return nxtque[pos];}
    	bool is_full(){
    		for(int i=0;i<32;i++){
    			if(!nxtque[i].busy) return 0;
    		}
    		return 1;
    	}
    	void clear(){
    		exfl=0;
    		for(int i=0;i<32;i++){
    			preque[i].clear();
				nxtque[i].clear();
    		}
		}
    };
    struct SLB_node{
    	int count;
    	RS_node rsnode;
    	bool is_commmited;
    	SLB_node():count(0),is_commmited(0){}
    	bool is_ready(){
//    		puts("here");printf("%d",rsnode.p==NULL);
    		if(rsnode.p->type==S) return rsnode.is_ready()&&is_commmited;
//    		puts("???");
    		return rsnode.is_ready();
		}
	};
	struct SLB{
		bool is_ok,is_store,nok,nstore;
		uint val,nval;
		int posROB,nROB;
		Queue<SLB_node>preque,nxtque;
		SLB():is_ok(0),is_store(0),nok(0),nstore(0),val(0),nval(0),posROB(0),nROB(0){}
		void update(){
			preque=nxtque;
			val=nval;
			is_ok=nok;
			is_store=nstore;
			posROB=nROB;
		}
		bool is_full(){return nxtque.get_status()==1;}
		bool is_empty(){return preque.get_status()==-1;}
		int push(const SLB_node&o){return nxtque.push(o);}
		void pop(){nxtque.pop();}
		SLB_node&operator[](int pos){return preque[pos];}
		SLB_node&operator()(int pos){return nxtque[pos];}
		void upload_commit(int pos){
			for(int i=nxtque.head;i!=nxtque.tail;){
				i=nxtque.nxt(i);
				if(nxtque[i].rsnode.id==pos) nxtque[i].is_commmited=1;
			}
		}
		void upload_rename(int pos,uint v){
			for(int i=nxtque.head;i!=nxtque.tail;){
				i=nxtque.nxt(i);
				nxtque[i].rsnode.match(pos,v);
			}
		}
// Questions here;
		void clear(){
			nok=0;
    		preque.clear();
    		while(nxtque.get_status()!=-1&&nxtque.get_front().rsnode.p->type==S&&nxtque.get_front().is_ready()){
    			preque.push(nxtque.get_front());
    			nxtque.pop();
    		}
    		nxtque.clear();
    		nxtque=preque;
		}
	};
	struct Issue_sig{
		bool has_res,toRS;
		uint _code;
		RS_node rsnode;
		Issue_sig():has_res(0){}
	};
	struct Execute_sig{
		bool has_res;
		uint val,npc;
		int posROB;
		Execute_sig():has_res(0),val(0),npc(0),posROB(0){}
	};
	struct Reg_sig{
		int issue_rd,issue_pos,commit_rd,id;
		uint val;
		Reg_sig():issue_rd(-1),issue_pos(-1),commit_rd(-1),val(0){}
		void clear(){
            issue_rd = issue_pos = commit_rd = -1;
            val=0;
		}
	};
	struct iss{
		uint _code,_npc,_pc;
	};
private:
	uint pc,nxt_pc,code_from_rob_to_commit;
	uint*mem;
	ROB_node carrier;
	_register prereg,nxtreg;
	Queue<iss>preque,nxtque;
	int commit_to_SLB_id;
    bool commit_flag, reserve_flag, reserve_isJump, reserve_isStore;
    bool fetch_flag, ROB_is_stall, commit_to_SLB;
    bool issue_to_ex_flag, new_issue_to_ex_flag;
    RS_node issue_to_ex_node, new_issue_to_ex_node;
    Issue_sig issue_sig;
    Execute_sig execute_sig;
    ROB rob;
    RS rs;
    SLB slb;
    Reg_sig reg_sig;
    
    char predict[4096], history[4096];
    char predictTable[4096][4];
    unsigned int HASH(unsigned int pc) { return ((pc >> 12) ^ (pc >> 2)) & 0xfff; }

public:
	Simulator():pc(0),nxt_pc(0),mem(new uint[500000]){
        new_issue_to_ex_flag = issue_to_ex_flag = commit_flag = commit_to_SLB = reserve_flag = reserve_isStore = reserve_isJump = fetch_flag = false;
        
        memset(mem, 0, sizeof(mem));
        for (int i = 0; i < 4096; ++i) predict[i] = 1;
        for (int i = 0; i < 4096; ++i)
            for (int j = 0; j < 3; ++j) predictTable[i][j] = 2;
        memset(history, 0, sizeof(history));
	}
	void scan(){
		char S[20];pc=0;
		while(scanf("%s",S)!=EOF){
			if(S[0]=='@'){
				pc=0;
				for(int i=1;i<=8;i++){
					pc=(pc<<4)+hextoint(S[i]);
				}
			}else{
				mem[pc++]=(hextoint(S[0])<<4)+hextoint(S[1]);
			}
		}
		pc=0;
	}
	
    void run() {
        while (true) {
//        	for(int i=0;i<32;i++) printf("%d ",prereg[i]);
//        	printf("   pc == %d    nxtpc == %d\n",pc,nxt_pc); 
//			puts(""); 
            run_rob();
            if (carrier._code == 0x0ff00513) {
                std::cout << std::dec << ((unsigned int) prereg[10] & 255u) << std::endl;
                break;
            }
//            puts("SLB");
            run_slbuffer();
//            puts("RS");
            run_reservation();
//            puts("reg");
            run_regfile();
//            puts("IQ");
            run_inst_fetch_queue();
//            puts("update");
            update();

//            puts("IS");
            run_issue();
//            puts("EX");
            run_ex();
//            puts("commit");
            run_commit();
        }
    }
    void run_inst_fetch_queue(){
    	pc=nxt_pc;
    	if(fetch_flag) nxtque.pop();
    	if(nxtque.get_status()!=1){
    		uint order=get_order(pc);
    		uint fea0=sep(order,0,6);
    		if(fea0==111){
    			uint imm=ext((sep(order,31,31)<<20)|(sep(order,21,30)<<1)|(sep(order,20,20)<<11)|(sep(order,12,19)<<12),20);
    			nxt_pc=pc+imm;
    		}else if(fea0==99){
    			uint imm=ext((sep(order,31,31)<<12)|(sep(order,25,30)<<5)|(sep(order,8,11)<<1)|(sep(order,7,7)<<11),12);
    			nxt_pc=(predictTable[HASH(pc)][history[HASH(pc)]]>=2)?pc+imm:pc+4;
    		}else nxt_pc=pc+4;
    		nxtque.push({order,nxt_pc,pc});
    	}
    }
    void run_issue(){
    	reserve_flag=issue_sig.has_res=fetch_flag=0;
    	if(preque.get_status()!=-1&&!rob.is_full()){
    		uint order=preque.get_front()._code;
    		issue_sig._code=order;
    		fetch_flag=1;
    		
    		bool illegal=0;
    		int Q1,Q2;
    		uint V1,V2,imm,npc=preque.get_front()._pc;
    		uint fea0=sep(order,0,6);
    		int rd=-1,rs1=-1,rs2=-1;
    		command*p=new command;Type type;
    		switch(fea0){
    			case 55:
    			case 23:
    					imm=sep(order,12,31)<<12;
    					rd=sep(order,7,11);
    					type=U;
    					break;
    			case 111:
    					imm=ext((sep(order,31,31)<<20)|(sep(order,21,30)<<1)|(sep(order,20,20)<<11)|(sep(order,12,19)<<12),20);
    					rd=sep(order,7,11);
    					type=J;
    					break;
    			case 99:
    					imm=ext((sep(order,31,31)<<12)|(sep(order,25,30)<<5)|(sep(order,8,11)<<1)|(sep(order,7,7)<<11),12);
						rs1=sep(order,15,19);
						rs2=sep(order,20,24);
    					type=B;
    					break;
    			case 3:
    			case 19:
    			case 103:
						imm=ext(sep(order,20,31),11);
						rd=sep(order,7,11);
						rs1=sep(order,15,19);
    					type=I;
    					break;
    			case 35:
						imm=ext((sep(order,25,31)<<5)|sep(order,7,11),11);
						rs1=sep(order,15,19);
						rs2=sep(order,20,24);
    					type=S;
    					break;
    			case 51:
						rd=sep(order,7,11);
						rs1=sep(order,15,19);
						rs2=sep(order,20,24);
    					type=R;
    					break;
    			default:
    					illegal=0;
    		}
    		if(!illegal){
    			p->set(order,imm,npc,type);
    			int pos=rob.getTail();
    			if(rd>0){
    				reg_sig.issue_rd=rd;
    				reg_sig.issue_pos=pos;
    			}else{
    				reg_sig.issue_rd=-1;
    			}
    			if(rs1!=-1){
    				if(!(Q1=prereg(rs1))) V1=prereg[rs1];
    				else if(rob[Q1].is_done){
    					V1=rob[Q1].val;
    					Q1=0;
    				}
    			}else Q1=0;
    			if(rs2!=-1){
    				if(!(Q2=prereg(rs2))) V2=prereg[rs2];
    				else if(rob[Q2].is_done){
    					V2=rob[Q2].val;
    					Q2=0;
    				}
    			}else Q2=0;
//    			printf("---------------%u\n",npc);
    			issue_sig.rsnode.set(1,pos,Q1,Q2,V1,V2,p);
//    			printf("---------------%u\n",issue_sig.rsnode.p->npc);
    			if(fea0==35||fea0==3) issue_sig.toRS=0;
    			else issue_sig.toRS=1;
    			issue_sig.has_res=reserve_flag=1;
    			issue_sig.rsnode._code=order;
    			
    			carrier.ppc=preque.get_front()._pc;
    			carrier.is_store=issue_sig.rsnode.p->type==S;
    			carrier._code=order;
    			carrier.rd=(rd?rd:-1);
    			carrier.is_jump=(fea0==99||fea0==103);
    			carrier.xpc=preque.get_front()._npc;
    			new_issue_to_ex_flag=0; 
    			if(issue_sig.toRS&&issue_sig.rsnode.is_ready()){
    				issue_sig.has_res=0;
                    new_issue_to_ex_flag=1;
                    new_issue_to_ex_node=issue_sig.rsnode;
    			}
    			if(issue_sig.toRS&&rs.is_full()||!issue_sig.toRS&&slb.is_full()){
    				issue_sig.has_res=0;
                    reserve_flag=0;
                    fetch_flag=0;
    			}
//    			printf("<new_issue_to_ex_node = %u>\n",new_issue_to_ex_node.p->npc);
    		}
    	}
    }
    void run_reservation(){
    	if(issue_sig.has_res&&issue_sig.toRS){
    		rs.insert(issue_sig.rsnode);
    	}
    	rs.exfl=rs.is_ready();
//    	if(!rs.exfl) puts("--------------------------------RS GG!");
    	if(rs.exfl&&new_issue_to_ex_flag){
    		rs.insert(new_issue_to_ex_node);
    		new_issue_to_ex_flag=0;
    	}
    	if(execute_sig.has_res){
//    		printf("---------------------%d is commiting in RS!\n",execute_sig.posROB);
    		for(int i=0;i<32;i++){
    			if(rs(i).busy) rs(i).match(execute_sig.posROB,execute_sig.val);
    		}
    	}
    	if(slb.is_ok){
//    		printf("slb------------------%d is commiting in RS!\n",slb.posROB);
    		for(int i=0;i<32;i++){
    			if(rs(i).busy) rs(i).match(slb.posROB,slb.val);
    		}
    	}
    }
    void run_ex(){
    	execute_sig.has_res=1;
    	if(issue_to_ex_flag){
//    		puts("issue to ex here");
    		RS_node&tmp=issue_to_ex_node;
    		execute_sig.posROB=tmp.id;
    		tmp.p->process(execute_sig.val,tmp.V1,tmp.V2);
    		execute_sig.npc=tmp.p->get_npc();
    		issue_to_ex_flag=0;
    	}else if(rs.exfl){
    		RS_node&tmp=rs.ex_node;
    		execute_sig.posROB=tmp.id;
    		tmp.p->process(execute_sig.val,tmp.V1,tmp.V2);
    		execute_sig.npc=tmp.p->get_npc();
    		rs.exfl=0;
    	}else execute_sig.has_res=0;
    }
    void run_slbuffer(){
    	slb.nok=0;
//            puts("1");
    	if(issue_sig.has_res&&!issue_sig.toRS){
    		SLB_node tmp;
    		tmp.rsnode=issue_sig.rsnode;
//    		printf("<SLB get order %u>    %d\n",tmp.rsnode._code,slb.is_empty());
    		if(slb.is_empty()){
//    			puts("empty");
    			if(tmp.is_ready()) ++tmp.count;
    		}
    		slb.push(tmp);
//    		printf("push de id is %d\n",slb.push(tmp));
//            puts("2");
    	}
    	if(!slb.is_empty()){
    		SLB_node&front=slb.preque.get_front();
//            puts("3");
    		if(front.is_ready()){
//            puts("3.5");
    			++front.count;
    			if(front.count==3){
//    				printf("@@@@@@@@@@@@@@@front here%u  %d\n",front.rsnode._code,front.rsnode.p->type==S);
    				uint st;
    				front.rsnode.p->process(st,front.rsnode.V1,front.rsnode.V2);
    				if(front.rsnode.p->type==S){
    					front.rsnode.p->Store(front.rsnode.V2,st,mem);
    					slb.nstore=1;
						slb.nok=0;
    				}else{
    					front.rsnode.p->Load(front.rsnode.V2,st,mem);
    					slb.nval=front.rsnode.V2;
    					slb.nstore=0;
    					slb.nok=1;
    				}
    				slb.nROB=front.rsnode.id;
    				slb.pop();
    			}else{
//            puts("4");
					slb.nxtque.get_front()=front;
				}
    		}
    	}
//            puts("11111");
    	if(commit_to_SLB) slb.upload_commit(commit_to_SLB_id);
    	if(execute_sig.has_res) slb.upload_rename(execute_sig.posROB,execute_sig.val);
    	if(slb.is_ok&&!slb.is_store) slb.upload_rename(slb.posROB,slb.val);
    }
    void run_rob(){
    	if(reserve_flag){
    		carrier.is_done=carrier.is_store;
    		rob.insert(carrier);
    		reserve_flag=0;
    	}
    	if(commit_flag){
    		rob.pop();
    		commit_flag=0;
    	}
    	if(execute_sig.has_res){
    		ROB_node&tmp=rob(execute_sig.posROB);
    		tmp.is_done=1;
    		tmp.val=execute_sig.val;
    		tmp.npc=execute_sig.npc;
    	}
    	if(slb.is_ok&&!slb.is_store){
    		ROB_node&tmp=rob(slb.posROB);
    		tmp.is_done=1;
    		tmp.val=slb.val;
    	}
    }
    void run_regfile(){
    	if(reg_sig.commit_rd!=-1){
    		nxtreg.reg[reg_sig.commit_rd]=reg_sig.val;
    		if(nxtreg.rname[reg_sig.commit_rd]==reg_sig.id) nxtreg.rname[reg_sig.commit_rd]=0;
    	}
    	if(reg_sig.issue_rd!=-1) nxtreg.rname[reg_sig.issue_rd]=reg_sig.issue_pos;
    	reg_sig.clear();
    }
    void run_commit(){
    	commit_to_SLB=commit_flag=0;
    	if(!rob.is_empty()){
    		ROB_node tmp=rob.get_front();
    		if(tmp.is_done){
//    		printf("order %u is done      %u\n",tmp._code,tmp.val);
    			commit_to_SLB=tmp.is_store;
    			commit_to_SLB_id=tmp.id;
    			reg_sig.commit_rd=tmp.rd;
    			reg_sig.val=tmp.val;
    			reg_sig.id=tmp.id;
    			commit_flag=1;
    			code_from_rob_to_commit=tmp._code;
    			if(sep(tmp._code,0,6)==99){//branch
    				if(tmp.npc!=tmp.ppc+4){
                        if (predictTable[HASH(tmp.ppc)][history[HASH(tmp.ppc)]]<3)
                            ++predictTable[HASH(tmp.ppc)][history[HASH(tmp.ppc)]];
    				}else if (predictTable[HASH(tmp.ppc)][history[HASH(tmp.ppc)]])
                        --predictTable[HASH(tmp.ppc)][history[HASH(tmp.ppc)]];
    				history[HASH(tmp.ppc)]=(history[HASH(tmp.ppc)]<<1)|(tmp.npc!=tmp.ppc+4);
                    history[HASH(tmp.ppc)]&=0b11;
    			}
    			if(tmp.is_jump&&tmp.xpc!=tmp.npc){
    				issue_sig.has_res=0;
    				new_issue_to_ex_flag=0;
    				issue_to_ex_flag=0;
    				reserve_flag=0;
    				carrier.is_store=carrier.is_jump=0;
    				fetch_flag=0;
    				execute_sig.has_res=0;
    				rs.exfl=0;
    				slb.is_ok=0;
    				rs.clear();
    				rob.clear();
    				slb.clear();
    				preque.clear();
    				nxtque.clear();
    				prereg.clear();
    				nxtreg.clear();
    				reg_sig.clear();
    				nxt_pc=tmp.npc;
    				commit_flag=0;
    			}
    		}
    	}
    }
    void update(){
    	prereg=nxtreg;
    	preque=nxtque;
        issue_to_ex_flag=new_issue_to_ex_flag;
        issue_to_ex_node=new_issue_to_ex_node;
        rs.update();
        slb.update();
        rob.update();
    }
    ~Simulator() {
    	delete []mem;
    }
};

int main(){
//	freopen("pi.data","r",stdin);
    std::ios::sync_with_stdio(false);
    Simulator ans;
    ans.scan();
    ans.run();
	return 0;
}
